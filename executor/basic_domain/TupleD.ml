module MakeJoinSemiLattice (T: DomainSpec.JoinSemiLatitce) (S: DomainSpec.JoinSemiLatitce) = struct
  type t = T.t * S.t
  let join (l1: t) (l2: t): t = (T.join (fst l1) (fst l2), S.join (snd l1) (snd l2))
  let le (l1: t) (l2: t): bool = (T.le (fst l1) (fst l2)) && (S.le (snd l1) (snd l2))
end

module MakeJoinSemiLatticeWithTop (T: DomainSpec.JoinSemiLatitceWithTop) (S: DomainSpec.JoinSemiLatitceWithTop) = struct
  include MakeJoinSemiLattice (T) (S)
  let top: t = (T.top, S.top)
end


module MakeLatticeWithTop (T: DomainSpec.LatticeWithTop) (S: DomainSpec.LatticeWithTop) = struct
  include MakeJoinSemiLattice (T) (S)
  let meet (l1: t) (l2: t): t = (T.meet (fst l1) (fst l2), S.meet (snd l1) (snd l2))
  let top: t = (T.top, S.top)
end


module MakeJoinSemiLattice_Record (T: DomainSpec.JoinSemiLatitce) (S: DomainSpec.JoinSemiLatitce) (R: sig
  type t
  val get_fst : t -> T.t
  val get_snd : t -> S.t
  val make : T.t -> S.t -> t
end) = struct
  type t = R.t
  let join (l1: t) (l2: t): t = R.make (T.join (R.get_fst l1) (R.get_fst l2)) (S.join (R.get_snd l1) (R.get_snd l2))
  let le (l1: t) (l2: t): bool = (T.le (R.get_fst l1) (R.get_fst l2)) && (S.le (R.get_snd l1) (R.get_snd l2))
end

module MakeLatticeWithTop_Record (T: DomainSpec.LatticeWithTop) (S: DomainSpec.LatticeWithTop) (R: sig
  type t
  val get_fst : t -> T.t
  val get_snd : t -> S.t
  val make : T.t -> S.t -> t
end) = struct
  type t = R.t
  let join (l1: t) (l2: t): t = R.make (T.join (R.get_fst l1) (R.get_fst l2)) (S.join (R.get_snd l1) (R.get_snd l2))
  let le (l1: t) (l2: t): bool = (T.le (R.get_fst l1) (R.get_fst l2)) && (S.le (R.get_snd l1) (R.get_snd l2))
  let top: t = R.make T.top S.top
end

