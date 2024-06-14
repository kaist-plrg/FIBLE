open StdlibExt

module type S = sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

module Make2 (Inst1 : S) (Inst2 : S) = struct
  type t = (Inst1.t, Inst2.t) Either.t

  let first (a : Inst1.t) : t = Either.left a
  let second (a : Inst2.t) : t = Either.right a

  let fold (first : Inst1.t -> 'a) (second : Inst2.t -> 'a) =
    Either.fold ~left:first ~right:second

  let pp fmt = fold (Inst1.pp fmt) (Inst2.pp fmt)
  let is_nop = fold Inst1.is_nop Inst2.is_nop
end

module Make3 (Inst1 : S) (Inst2 : S) (Inst3 : S) = struct
  type t = (Inst1.t, Inst2.t, Inst3.t) Either3.t

  let t_of_sexp = function
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "first"; sexp ] ->
        Either3.first (Inst1.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "second"; sexp ] ->
        Either3.second (Inst2.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "third"; sexp ] ->
        Either3.third (Inst3.t_of_sexp sexp)
    | _ -> Sexplib.Conv_error.no_variant_match ()

  let sexp_of_t = function
    | Either3.First a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "first"; Inst1.sexp_of_t a ]
    | Either3.Second a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "second"; Inst2.sexp_of_t a ]
    | Either3.Third a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "third"; Inst3.sexp_of_t a ]

  let first (a : Inst1.t) : t = Either3.first a
  let second (a : Inst2.t) : t = Either3.second a
  let third (a : Inst3.t) : t = Either3.third a

  let fold (first : Inst1.t -> 'a) (second : Inst2.t -> 'a)
      (third : Inst3.t -> 'a) =
    Either3.fold first second third

  let pp fmt = fold (Inst1.pp fmt) (Inst2.pp fmt) (Inst3.pp fmt)
  let is_nop = fold Inst1.is_nop Inst2.is_nop Inst3.is_nop
end

module Make4 (Inst1 : S) (Inst2 : S) (Inst3 : S) (Inst4 : S) = struct
  type t = (Inst1.t, Inst2.t, Inst3.t, Inst4.t) Either4.t

  let t_of_sexp = function
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "first"; sexp ] ->
        Either4.first (Inst1.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "second"; sexp ] ->
        Either4.second (Inst2.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "third"; sexp ] ->
        Either4.third (Inst3.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "fourth"; sexp ] ->
        Either4.fourth (Inst4.t_of_sexp sexp)
    | _ -> Sexplib.Conv_error.no_variant_match ()

  let sexp_of_t = function
    | Either4.First a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "first"; Inst1.sexp_of_t a ]
    | Either4.Second a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "second"; Inst2.sexp_of_t a ]
    | Either4.Third a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "third"; Inst3.sexp_of_t a ]
    | Either4.Fourth a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "fourth"; Inst4.sexp_of_t a ]

  let first (a : Inst1.t) : t = Either4.first a
  let second (a : Inst2.t) : t = Either4.second a
  let third (a : Inst3.t) : t = Either4.third a
  let fourth (a : Inst4.t) : t = Either4.fourth a

  let fold (first : Inst1.t -> 'a) (second : Inst2.t -> 'a)
      (third : Inst3.t -> 'a) (fourth : Inst4.t -> 'a) =
    Either4.fold first second third fourth

  let pp fmt = fold (Inst1.pp fmt) (Inst2.pp fmt) (Inst3.pp fmt) (Inst4.pp fmt)
  let is_nop = fold Inst1.is_nop Inst2.is_nop Inst3.is_nop Inst4.is_nop
end

module Make7
    (Inst1 : S)
    (Inst2 : S)
    (Inst3 : S)
    (Inst4 : S)
    (Inst5 : S)
    (Inst6 : S)
    (Inst7 : S) =
struct
  type t =
    (Inst1.t, Inst2.t, Inst3.t, Inst4.t, Inst5.t, Inst6.t, Inst7.t) Either7.t

  let t_of_sexp = function
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "first"; sexp ] ->
        Either7.first (Inst1.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "second"; sexp ] ->
        Either7.second (Inst2.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "third"; sexp ] ->
        Either7.third (Inst3.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "fourth"; sexp ] ->
        Either7.fourth (Inst4.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "fifth"; sexp ] ->
        Either7.fifth (Inst5.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "sixth"; sexp ] ->
        Either7.sixth (Inst6.t_of_sexp sexp)
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "seventh"; sexp ] ->
        Either7.seventh (Inst7.t_of_sexp sexp)
    | _ -> Sexplib.Conv_error.no_variant_match ()

  let sexp_of_t = function
    | Either7.First a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "first"; Inst1.sexp_of_t a ]
    | Either7.Second a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "second"; Inst2.sexp_of_t a ]
    | Either7.Third a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "third"; Inst3.sexp_of_t a ]
    | Either7.Fourth a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "fourth"; Inst4.sexp_of_t a ]
    | Either7.Fifth a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "fifth"; Inst5.sexp_of_t a ]
    | Either7.Sixth a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "sixth"; Inst6.sexp_of_t a ]
    | Either7.Seventh a ->
        Sexplib.Sexp.List [ Sexplib.Sexp.Atom "seventh"; Inst7.sexp_of_t a ]

  let first (a : Inst1.t) : t = Either7.first a
  let second (a : Inst2.t) : t = Either7.second a
  let third (a : Inst3.t) : t = Either7.third a
  let fourth (a : Inst4.t) : t = Either7.fourth a
  let fifth (a : Inst5.t) : t = Either7.fifth a
  let sixth (a : Inst6.t) : t = Either7.sixth a
  let seventh (a : Inst7.t) : t = Either7.seventh a

  let fold (first : Inst1.t -> 'a) (second : Inst2.t -> 'a)
      (third : Inst3.t -> 'a) (fourth : Inst4.t -> 'a) (fifth : Inst5.t -> 'a)
      (sixth : Inst6.t -> 'a) (seventh : Inst7.t -> 'a) =
    Either7.fold first second third fourth fifth sixth seventh

  let pp fmt =
    fold (Inst1.pp fmt) (Inst2.pp fmt) (Inst3.pp fmt) (Inst4.pp fmt)
      (Inst5.pp fmt) (Inst6.pp fmt) (Inst7.pp fmt)

  let is_nop =
    fold Inst1.is_nop Inst2.is_nop Inst3.is_nop Inst4.is_nop Inst5.is_nop
      Inst6.is_nop Inst7.is_nop
end
