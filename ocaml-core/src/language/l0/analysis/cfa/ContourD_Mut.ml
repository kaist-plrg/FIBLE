open Basic_domain

type __ = { unsound_jump : UJumpD_Mut.t; boundary_point : BoundaryPointD_Mut.t }

include
  TupleD.MakeJoinSemiLattice_Mut_Record (UJumpD_Mut) (BoundaryPointD_Mut)
    (struct
      type t = __

      let get_fst (x : t) = x.unsound_jump
      let get_snd (x : t) = x.boundary_point
    end)
