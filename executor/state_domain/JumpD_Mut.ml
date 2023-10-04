open Basic
open Basic_domain
include LocBotMapD.Make_Mut (LocSetD)

let join_single_loc (a : t) (loc : Loc.t) (v : LocSetD.t) =
  let old_v = find_opt loc a in
  let new_v =
    Option.map (fun ov -> LocSetD.join ov v) old_v |> Option.value ~default:v
  in
  add loc new_v a

let get_preds (a : t) (loc : Loc.t) : Loc.t List.t =
  fold (fun k v acc -> if LocSetD.mem loc v then k :: acc else acc) a []
