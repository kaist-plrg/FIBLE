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

let top = { regs = ARegFile.top; stack = AStack.top }
let le c1 c2 = ARegFile.le c1.regs c2.regs && AStack.le c1.stack c2.stack
let widen = join

let eval_varnode (c : t) (vn : VarNode.t) : ASymb.t =
  match vn with
  | VarNode.Const c -> ASymb.Top
  | VarNode.Register r -> ARegFile.get c.regs r.id
  | VarNode.Ram _ -> ASymb.Top

let process_assignment (c : t) (a : Assignable.t) (i : RegId.t_full) : t =
  match a with
  | Assignable.Avar vn ->
      { c with regs = ARegFile.add c.regs i.id (eval_varnode c vn) }
  | Auop _ -> { c with regs = ARegFile.add c.regs i.id Top }
  | Abop _ -> { c with regs = ARegFile.add c.regs i.id Top }

let process_sload (c : t) (offset : Const.t) (o : RegId.t_full) =
  let v = AStack.process_sload c.stack offset.value in
  { c with regs = ARegFile.add c.regs o.id v }

let process_sstore (c : t) (offset : Const.t) (i : ASymb.t) =
  { c with stack = AStack.process_sstore c.stack offset.value i }

let post_single_instr (i : Inst.t) (c : t) : t =
  match i with
  | Inst.INop -> c
  | Inst.Iassignment { expr; output } -> process_assignment c expr output
  | Inst.Iload _ -> c
  | Inst.Istore _ -> c
  | Inst.Isload { offset; output } -> process_sload c offset output
  | Inst.Isstore { offset; value } ->
      process_sstore c offset (eval_varnode c value)

let post_single_jmp (i : Jmp.t) (c : t) : t =
  match i with
  | Jmp.Jcall _ | Jmp.Jcall_ind _ -> { c with regs = ARegFile.top }
  | _ -> c

let post_single_block (b : Block.t) (c : t) : t =
  Block.fold_left (fun c i -> post_single_instr i.ins c) c b |> fun c ->
  post_single_jmp b.jmp.jmp c
