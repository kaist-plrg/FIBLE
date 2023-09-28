open Basic_domain;;

type __ = {
      unsound_jump: UJumpD.t;
      boundary_point: BoundaryPointD.t
}

include TupleD.MakeJoinSemiLattice_Record(UJumpD)(BoundaryPointD)(struct
    type t = __
    let get_fst (x: t) = x.unsound_jump
    let get_snd (x: t) = x.boundary_point
    let make x y = {unsound_jump = x; boundary_point = y}
  end)