type t = L1 of FGIR.Syn.Prog.t | L2 of ASIR.Syn.Prog.t | L3 of IOIR.Syn.Prog.t
[@@deriving sexp, show]

type symbol_table = {
  funcs : (Int64.t * String.t) List.t;
  objects : (Int64.t * String.t) List.t;
}
[@@deriving sexp, show]
