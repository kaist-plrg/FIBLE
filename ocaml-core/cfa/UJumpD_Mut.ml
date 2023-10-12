open Basic
open Basic_domain
include LocBotMapD.Make_Mut (LocSetOptionD)

let join_single_loc (a : t) (loc : Loc.t) (v : LocSetOptionD.t) =
  let old_v = find_opt loc a in
  let new_v =
    Option.map (fun ov -> LocSetOptionD.join ov v) old_v
    |> Option.value ~default:v
  in
  add loc new_v a
