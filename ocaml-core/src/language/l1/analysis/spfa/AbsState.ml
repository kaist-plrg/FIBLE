open StdlibExt
open Common
open Basic_domain
open Value_domain

type t = { regs : ARegFile.t; stack : AStack.t }

let eval_varnode (c : t) (vn : VarNode.t) : AbsVal.t =
  match vn with
  | VarNode.Const c -> AbsVal.of_const (NumericValue.of_int64 c.value c.width)
  | VarNode.Register r -> ARegFile.get c.regs r.id
  | VarNode.Ram _ -> AbsVal.Top

let process_assignment (c : t) (a : Assignable.t) (i : RegId.t_full) : t =
  match a with
  | Assignable.Avar vn ->
      { c with regs = ARegFile.add c.regs i.id (eval_varnode c vn) }
  | Auop (uop, vn) ->
      {
        c with
        regs =
          ARegFile.add c.regs i.id
            (AbsVal.eval_uop uop (eval_varnode c vn) i.width);
      }
  | Abop (bop, lvn, rvn) ->
      {
        c with
        regs =
          ARegFile.add c.regs i.id
            (AbsVal.eval_bop bop (eval_varnode c lvn) (eval_varnode c rvn)
               i.width);
      }

let process_load (c : t) (ptr : AbsVal.t) (o : RegId.t_full) =
  let v = AStack.process_load c.stack ptr in
  { c with regs = ARegFile.add c.regs o.id v }

let process_store (c : t) (ptr : AbsVal.t) (i : AbsVal.t) =
  { c with stack = AStack.process_store c.stack ptr i }

let join a b =
  { regs = ARegFile.join a.regs b.regs; stack = AStack.join a.stack b.stack }

let post_single_instr (i : Inst.t) (c : t) : AccessD.t * t =
  match i with
  | IN _ -> (AccessD.bottom, c)
  | IA { expr; output } -> (AccessD.bottom, process_assignment c expr output)
  | ILS (Load { pointer; output; _ }) ->
      let pointer = eval_varnode c pointer in
      [%log debug "load: %a" AbsVal.pp pointer];
      (AccessD.log_access output.width pointer, process_load c pointer output)
  | ILS (Store { pointer; value }) ->
      let pointer = eval_varnode c pointer in
      let value' = eval_varnode c value in
      [%log debug "store: %a" AbsVal.pp pointer];
      ( AccessD.log_access (VarNode.width value) pointer,
        process_store c pointer value' )

let post_single_jmp (i : Jmp.t) (c : t) (sp_num : Int32.t) : t =
  match i with
  | JC _ ->
      {
        c with
        regs =
          ARegFile.add c.regs (RegId.Register sp_num)
            (AbsVal.eval_bop Bint_add
               (ARegFile.get c.regs (RegId.Register sp_num))
               (AbsVal.of_const (NumericValue.of_int64 8L 8l))
               8l);
      }
  | _ -> c

let post_single_block (b : Block.t) (c : t) (sp_num : Int32.t) : AccessD.t * t =
  Block.fold_left
    (fun (acc, c) i ->
      let nacc, c = post_single_instr i.ins c in
      (AccessD.join acc nacc, c))
    (AccessD.bottom, c) b
  |> fun (ac, c) -> (ac, post_single_jmp b.jmp.jmp c sp_num)

let widen = join
let le (a : t) (b : t) = AStack.le a.stack b.stack && ARegFile.le a.regs b.regs
let top = { regs = ARegFile.top; stack = AStack.top }
