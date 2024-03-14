open StdlibExt
open Common
open Basic_domain
open Value_domain
include NonRelStateD

let post_single_instr (i : Inst.t) (c : t) : AccessD.t * t =
  match i with
  | IN _ -> (AccessD.bottom, c)
  | IA { expr; output } -> (AccessD.bottom, process_assignment c expr output)
  | ILS (Load { pointer; output; _ }) ->
      [%log debug "load: %a" AbsVal.pp (eval_varnode c pointer)];
      (AccessD.log_access output.width (eval_varnode c pointer), c)
  | ILS (Store { pointer; value }) ->
      [%log debug "store: %a" AbsVal.pp (eval_varnode c pointer)];
      (AccessD.log_access (VarNode.width value) (eval_varnode c pointer), c)

let post_single_jmp (i : Jmp.t) (c : t) (sp_num : Int32.t) : t =
  match i with
  | JC _ ->
      add (RegId.Register sp_num)
        (AbsVal.add
           (eval_varnode c
              (VarNode.Register
                 { id = RegId.Register sp_num; offset = 0l; width = 8l }))
           (AbsVal.of_const 8L) 8L)
        c
  | _ -> c

let post_single_block (b : Block.t) (c : t) (sp_num : Int32.t) : AccessD.t * t =
  Block.fold_left
    (fun (acc, c) i ->
      let nacc, c = post_single_instr i.ins c in
      (AccessD.join acc nacc, c))
    (AccessD.bottom, c) b
  |> fun (ac, c) -> (ac, post_single_jmp b.jmp.jmp c sp_num)

let widen = join
