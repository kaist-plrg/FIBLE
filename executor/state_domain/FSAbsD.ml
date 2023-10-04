open Basic
open Basic_domain
module AbsLocMapD = BotMapD.Make (Loc) (AbsState)

type __ = { pre_state : AbsLocMapD.t; post_state : AbsLocMapD.t }

include
  TupleD.MakeJoinSemiLattice_Record (AbsLocMapD) (AbsLocMapD)
    (struct
      type t = __

      let get_fst x = x.pre_state
      let get_snd x = x.post_state
      let make x y = { pre_state = x; post_state = y }
    end)
