open Common
open Basic_domain
include TupleD.MakeJoinSemiLattice (LocSetD) (LocSetD)

let add_entry (a : t) (l : Loc.t) : t = (LocSetD.add l (fst a), snd a)
let add_exit (a : t) (l : Loc.t) : t = (fst a, LocSetD.add l (snd a))
