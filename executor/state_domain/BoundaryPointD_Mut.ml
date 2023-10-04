open Basic
open Basic_domain
include TupleD.MakeJoinSemiLattice_Mut (LocSetD_Mut) (LocSetD_Mut)

let add_entry (a : t) (l : Loc.t) : unit = LocSetD_Mut.add l (fst a)
let add_exit (a : t) (l : Loc.t) : unit = LocSetD_Mut.add l (snd a)
