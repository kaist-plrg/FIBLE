module type PoSet = sig
  type t

  val le : t -> t -> bool
end

module type JoinSemiLatitce = sig
  include PoSet

  val join : t -> t -> t
end

module type MeetSemiLattice = sig
  include PoSet

  val meet : t -> t -> t
end

module type JoinSemiLatitceWithTop = sig
  include JoinSemiLatitce

  val top : t
end

module type MeetDomainWithBot = sig
  include MeetSemiLattice

  val bot : t
end

module type Lattice = sig
  include JoinSemiLatitce
  include MeetSemiLattice with type t := t
end

module type LatticeWithTop = sig
  include Lattice

  val top : t
end

module type LatticeWithBot = sig
  include Lattice

  val bot : t
end

module type LatticeWithTopAndBot = sig
  include Lattice

  val top : t
  val bot : t
end

module type JoinSemiLatitce_Mut = sig
  include PoSet

  val join : t -> t -> unit
end

module type MeetSemiLattice_Mut = sig
  include PoSet

  val meet : t -> t -> unit
end

module type Lattice_Mut = sig
  include JoinSemiLatitce_Mut
  include MeetSemiLattice_Mut with type t := t
end

module type LatticeWithTop_Mut = sig
  include Lattice_Mut

  val top : t
end

module type LatticeWithBot_Mut = sig
  include Lattice_Mut

  val bot : t
end

module type LatticeWithTopAndBot_Mut = sig
  include Lattice_Mut

  val top : t
  val bot : t
end
