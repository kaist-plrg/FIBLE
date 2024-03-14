module MakeJoinSemiLattice_Record
    (T1 : DomainSpec.JoinSemiLatitce)
    (T2 : DomainSpec.JoinSemiLatitce)
    (T3 : DomainSpec.JoinSemiLatitce)
    (R : sig
      type t

      val get_fst : t -> T1.t
      val get_snd : t -> T2.t
      val get_trd : t -> T3.t
      val make : T1.t -> T2.t -> T3.t -> t
    end) =
struct
  type t = R.t

  let join (l1 : t) (l2 : t) : t =
    R.make
      (T1.join (R.get_fst l1) (R.get_fst l2))
      (T2.join (R.get_snd l1) (R.get_snd l2))
      (T3.join (R.get_trd l1) (R.get_trd l2))

  let le (l1 : t) (l2 : t) : bool =
    T1.le (R.get_fst l1) (R.get_fst l2)
    && T2.le (R.get_snd l1) (R.get_snd l2)
    && T3.le (R.get_trd l1) (R.get_trd l2)
end

module MakeJoinSemiLatticeWithTop_Record
    (T1 : DomainSpec.JoinSemiLatitceWithTop)
    (T2 : DomainSpec.JoinSemiLatitceWithTop)
    (T3 : DomainSpec.JoinSemiLatitceWithTop)
    (R : sig
      type t

      val get_fst : t -> T1.t
      val get_snd : t -> T2.t
      val get_trd : t -> T3.t
      val make : T1.t -> T2.t -> T3.t -> t
    end) =
struct
  include MakeJoinSemiLattice_Record (T1) (T2) (T3) (R)

  let top : t = R.make T1.top T2.top T3.top
end

module MakeLattice_Record
    (T1 : DomainSpec.Lattice)
    (T2 : DomainSpec.Lattice)
    (T3 : DomainSpec.Lattice)
    (R : sig
      type t

      val get_fst : t -> T1.t
      val get_snd : t -> T2.t
      val get_trd : t -> T3.t
      val make : T1.t -> T2.t -> T3.t -> t
    end) =
struct
  include MakeJoinSemiLattice_Record (T1) (T2) (T3) (R)

  let meet (l1 : t) (l2 : t) : t =
    R.make
      (T1.meet (R.get_fst l1) (R.get_fst l2))
      (T2.meet (R.get_snd l1) (R.get_snd l2))
      (T3.meet (R.get_trd l1) (R.get_trd l2))
end

module MakeLatticeWithTop_Record
    (T1 : DomainSpec.LatticeWithTop)
    (T2 : DomainSpec.LatticeWithTop)
    (T3 : DomainSpec.LatticeWithTop)
    (R : sig
      type t

      val get_fst : t -> T1.t
      val get_snd : t -> T2.t
      val get_trd : t -> T3.t
      val make : T1.t -> T2.t -> T3.t -> t
    end) =
struct
  include MakeLattice_Record (T1) (T2) (T3) (R)

  let top : t = R.make T1.top T2.top T3.top
end

module MakeJoinSemiLattice_Mut_Record
    (T1 : DomainSpec.JoinSemiLatitce_Mut)
    (T2 : DomainSpec.JoinSemiLatitce_Mut)
    (T3 : DomainSpec.JoinSemiLatitce_Mut)
    (R : sig
      type t

      val get_fst : t -> T1.t
      val get_snd : t -> T2.t
      val get_trd : t -> T3.t
    end) =
struct
  type t = R.t

  let join (l1 : t) (l2 : t) : unit =
    T1.join (R.get_fst l1) (R.get_fst l2);
    T2.join (R.get_snd l1) (R.get_snd l2);
    T3.join (R.get_trd l1) (R.get_trd l2)

  let le (l1 : t) (l2 : t) : bool =
    T1.le (R.get_fst l1) (R.get_fst l2)
    && T2.le (R.get_snd l1) (R.get_snd l2)
    && T3.le (R.get_trd l1) (R.get_trd l2)
end
