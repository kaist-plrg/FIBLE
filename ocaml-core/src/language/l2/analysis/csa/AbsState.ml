open Basic
open Basic_collection

type t = { regs : ARegFile.t; stack : AStack.t }

let join c1 c2 =
  {
    regs = ARegFile.join c1.regs c2.regs;
    stack = AStack.join c1.stack c2.stack;
  }

let init =
  {
    regs = ARegFile.InitHoleMap RegIdMap.empty;
    stack = AStack.InitHoleMap AddrMap.empty;
  }

let le c1 c2 = ARegFile.le c1.regs c2.regs && AStack.le c1.stack c2.stack
let widen = join

let eval_varnode (c : t) (vn : VarNode.t) : ASymb.t =
  match vn with
  | VarNode.Const c -> ASymb.Top
  | VarNode.Register r -> ARegFile.get c.regs r.id

let process_assignment (c : t) (a : Assignable.t) (i : RegId.t_width) : t =
  match a with
  | Assignable.Avar vn ->
      { c with regs = ARegFile.add c.regs i.id (eval_varnode c vn) }
  | Auop _ -> { c with regs = ARegFile.add c.regs i.id Top }
  | Abop _ -> { c with regs = ARegFile.add c.regs i.id Top }

let process_sload (c : t) (offset : Const.t) (o : RegId.t_width) =
  let v = AStack.process_sload c.stack offset.value in
  { c with regs = ARegFile.add c.regs o.id v }

let process_sstore (c : t) (offset : Const.t) (i : ASymb.t) =
  { c with stack = AStack.process_sstore c.stack offset.value i }

let post_single_instr (i : Inst.t) (c : t) : t =
  match i with
  | Inst.INop -> c
  | Inst.Iassignment (a, vn) -> process_assignment c a vn
  | Inst.Iload (i0, i1, o) -> c
  | Inst.Istore (i0, i1, i2) -> c
  | Inst.Isload (offset, o) -> process_sload c offset o
  | Inst.Isstore (offset, i) -> process_sstore c offset (eval_varnode c i)

let post_single_jmp (i : Jmp.t) (c : t) (sp_num : int64) : t =
  match i with
  | Jmp.Jcall (d, _, _) | Jmp.Jcall_ind (d, _, _) ->
      { c with regs = ARegFile.top }
  | _ -> c

let post_single_block (b : Block.t) (c : t) (sp_num : int64) : t =
  Block.fold_left (fun c i -> post_single_instr i.ins c) c b |> fun c ->
  post_single_jmp b.jmp.jmp c sp_num
