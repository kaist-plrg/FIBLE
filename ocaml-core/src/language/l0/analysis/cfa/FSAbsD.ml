open Basic
open Basic_domain

module AbsLocMapD = struct
  include BotMapD.Make (Loc) (AbsState)

  let pp fmt a =
    Format.pp_print_list
      (fun fmt (k, v) -> Format.fprintf fmt "%a -> %a" Loc.pp k AbsState.pp v)
      fmt (bindings a)
end

type __ = { pre_state : AbsLocMapD.t; post_state : AbsLocMapD.t }

include
  TupleD.MakeJoinSemiLattice_Record (AbsLocMapD) (AbsLocMapD)
    (struct
      type t = __

      let get_fst x = x.pre_state
      let get_snd x = x.post_state
      let make x y = { pre_state = x; post_state = y }
    end)

let join_single_post (a : t) (loc : Loc.t) (v : AbsState.t) : t =
  let old_v = AbsLocMapD.find_opt loc a.post_state in
  let new_v =
    Option.map (fun ov -> AbsState.join ov v) old_v |> Option.value ~default:v
  in
  {
    pre_state = a.pre_state;
    post_state = AbsLocMapD.add loc new_v a.post_state;
  }

let pp fmt a =
  Format.fprintf fmt "@[<v 0>pre_state:@,  @[%a@]@,post_state:@,  @[%a@]@]"
    AbsLocMapD.pp a.pre_state AbsLocMapD.pp a.post_state

let pp_loc fmt (a, loc) =
  Format.fprintf fmt "@[<v 0>pre_state:@,  @[%a@]@,post_state:@,  @[%a@]@]"
    (Format.pp_print_option AbsState.pp)
    (AbsLocMapD.find_opt loc a.pre_state)
    (Format.pp_print_option AbsState.pp)
    (AbsLocMapD.find_opt loc a.post_state)
