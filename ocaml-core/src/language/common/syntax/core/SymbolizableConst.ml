type t = Num of NumericConst.t | Symbol of SymbolicConst.t [@@deriving sexp]

let get_width = function
  | Num n -> NumericConst.get_width n
  | Symbol s -> SymbolicConst.get_width s

let pp fmt = function
  | Num n -> NumericConst.pp fmt n
  | Symbol s -> SymbolicConst.pp fmt s
