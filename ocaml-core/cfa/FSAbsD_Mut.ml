open Basic
open Basic_domain
module AbsLocMapD = BotMapD.Make_Mut (Loc) (AbsState)

type __ = { pre_state : AbsLocMapD.t; post_state : AbsLocMapD.t }

include
  TupleD.MakeJoinSemiLattice_Mut_Record (AbsLocMapD) (AbsLocMapD)
    (struct
      type t = __

      let get_fst x = x.pre_state
      let get_snd x = x.post_state
    end)

let join_single_pre (a : t) (loc : Loc.t) (v : AbsState.t) =
  let old_v = AbsLocMapD.find_opt loc a.pre_state in
  let new_v =
    Option.map (fun ov -> AbsState.join ov v) old_v |> Option.value ~default:v
  in
  AbsLocMapD.add loc new_v a.pre_state

let join_single_post (a : t) (loc : Loc.t) (v : AbsState.t) =
  let old_v = AbsLocMapD.find_opt loc a.post_state in
  let new_v =
    Option.map (fun ov -> AbsState.join ov v) old_v |> Option.value ~default:v
  in
  AbsLocMapD.add loc new_v a.post_state
