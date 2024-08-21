open Common
include Set.Make (AExpr)

let find_filter_opt (f : elt -> bool) (s : t) : elt option =
  fold (fun x acc -> if f x then Some x else acc) s None

let pp fmt s =
  Format.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space AExpr.pp)
    (elements s)

let used_regs (s : t) : RegIdFullSet.t =
  fold
    (fun (k : AExpr.t) (acc : RegIdFullSet.t) -> RegIdFullSet.add k.base acc)
    s RegIdFullSet.empty

let has_same_diff (a : t) (b : t) (base : RegId.t_full) (target : RegId.t_full)
    : Bool.t =
  let abase, atarget =
    ( find_filter_opt (fun k -> k.base = base) a,
      find_filter_opt (fun k -> k.base = target) a )
  in
  let bbase, btarget =
    ( find_filter_opt (fun k -> k.base = base) b,
      find_filter_opt (fun k -> k.base = target) b )
  in
  match (abase, atarget, bbase, btarget) with
  | Some abase, Some atarget, Some bbase, Some btarget ->
      let a_diff = Z.sub atarget.offset abase.offset in
      let b_diff = Z.sub btarget.offset bbase.offset in
      Z.equal a_diff b_diff
  | _ -> false
