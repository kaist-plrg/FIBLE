open Basic
open Basic_domain

module A = struct
  include TupleD.MakeJoinSemiLatticeWithTop (OctagonD) (OctagonD)

  let pp fmt (oct, ocf) =
    Format.fprintf fmt "(%a, %a)" OctagonD.pp oct OctagonD.pp ocf
end

include MemRefTopMapD.Make (A)

let clear_memref a =
  filter (fun k _ -> match k with MemRef.R _ -> true | _ -> false) a

let clear_mr a (mr : MemRef.t) = filter (fun k _ -> Stdlib.compare k mr <> 0) a

let process_assignment (a : t) (d : OctagonD.t) (asn : Assignable.t)
    (outv : RegId.t) =
  match MemRef.convert_regid outv with
  | outmr -> (
      let na = clear_mr a outmr in
      match asn with
      | Avar vn ->
          (MemRef.convert_varnode vn |> Option.bind) (fun mr -> find_opt mr na)
          |> Option.map (fun v -> add outmr v na)
          |> Option.value ~default:na
      | Abop (Bint_less, op1v, op2v) -> (
          match (op1v, op2v) with
          | Register ({ id = RegId.Unique _; _ } as u), Const { value = c; _ }
            ->
              (add outmr
                 ( OctagonD.gen_single_lt
                     (OctagonD.gen_single_ge OctagonD.top (MemRef.R u) 0L)
                     (MemRef.R u) c,
                   OctagonD.gen_single_ge OctagonD.top (MemRef.R u) c ))
                na
          | _ -> na)
      | Abop (Bint_equal, op1v, op2v) -> (
          match (op1v, op2v) with
          | Register ({ id = RegId.Unique _; _ } as u), Const { value = c; _ }
            ->
              (add outmr
                 ( OctagonD.gen_single_eq OctagonD.top (MemRef.R u) c,
                   OctagonD.top ))
                na
          | _ -> na)
      | Abop (Bint_sless, _, _) -> na
      | Abop (Bint_slessequal, _, _) -> na
      | Abop (Bbool_or, op1v, op2v) -> (
          match (op1v, op2v) with
          | ( Register ({ id = RegId.Register _; _ } as r1),
              Register ({ id = RegId.Register _; _ } as r2) ) -> (
              match (find_opt (MemRef.R r1) na, find_opt (MemRef.R r2) na) with
              | Some (dt, df), Some (dt2, df2) ->
                  add outmr
                    ( OctagonD.join
                        (OctagonD.refine_consts (OctagonD.meet dt d))
                        (OctagonD.refine_consts (OctagonD.meet dt2 d)),
                      OctagonD.meet df df2 )
                    na
              | _ -> na)
          | _ -> na)
      | Abop (_, _, _) -> na
      | Auop (Ubool_negate, opv) -> (
          match opv with
          | Register ({ id = RegId.Unique _; _ } as u) -> (
              match find_opt (MemRef.R u) na with
              | Some (d1, d2) -> add outmr (d2, d1) na
              | _ -> na)
          | _ -> na)
      | Auop (_, _) -> na)
