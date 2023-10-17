open StdlibExt
open Basic
open Basic_domain
open Value_domain
include NonRelStateD

let print_endline = Interaction.print_endline

let post_single_instr (i : L1.Inst.t) (c : t) : AccessD.t * t =
  match i with
  | L1.Inst.INop -> (AccessD.bottom, c)
  | L1.Inst.Iassignment (a, vn) -> (AccessD.bottom, process_assignment c a vn)
  | L1.Inst.Iload (i0, i1, o) ->
      Format.asprintf "load: %a" SPVal.pp (eval_varnode c i1) |> print_endline;
      (AccessD.log_access (eval_varnode c i1), c)
  | L1.Inst.Istore (i0, i1, i2) ->
      Format.asprintf "store: %a\n" SPVal.pp (eval_varnode c i1)
      |> print_endline;
      (AccessD.log_access (eval_varnode c i1), c)

let post_single_jmp (i : L1.Jmp.t) (c : t) (sp_num : int64) : t =
  match i with
  | L1.Jmp.Jcall _ | L1.Jmp.Jcall_ind _ ->
      add
        { id = RegId.Register sp_num; width = 8l }
        (SPVal.add
           (eval_varnode c
              (VarNode.Register { id = RegId.Register sp_num; width = 8l }))
           (SPVal.of_const 8L) 8L)
        c
  | _ -> c

let post_single_block (b : L1.Block.t) (c : t) (sp_num : int64) : AccessD.t * t
    =
  L1.Block.fold_left
    (fun (acc, c) i ->
      let nacc, c = post_single_instr i.ins c in
      (AccessD.join acc nacc, c))
    (AccessD.bottom, c) b
  |> fun (ac, c) -> (ac, post_single_jmp b.jmp.jmp c sp_num)

let widen = join
