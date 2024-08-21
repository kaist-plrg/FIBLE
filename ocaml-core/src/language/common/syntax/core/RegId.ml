open Sexplib.Std

type t = Unique of int32 | Register of int32

let t_of_sexp (v : Sexplib.Sexp.t) : t =
  match v with
  | List [ Atom "Unique"; v ] -> Unique (Int32.t_of_sexp_hex v)
  | List [ Atom "Register"; v ] -> Register (Int32.t_of_sexp_hex v)
  | _ -> Sexplib.Conv_error.no_variant_match ()

let sexp_of_t (v : t) : Sexplib.Sexp.t =
  match v with
  | Unique v -> List [ Atom "Unique"; Int32.sexp_of_t_hex v ]
  | Register v -> List [ Atom "Register"; Int32.sexp_of_t_hex v ]

type t_full = { id : t; offset : int32; width : int32 } [@@deriving sexp]

let width v = v.width

let pp (fmt : Format.formatter) (v : t) =
  match v with
  | Register n -> Format.fprintf fmt "$%ld" n
  | Unique n -> Format.fprintf fmt "$U%lx" n

let pp_full (fmt : Format.formatter) (v : t_full) =
  Format.fprintf fmt "%a(%ld:%ld)" pp v.id v.offset v.width

let compare (a : t) (b : t) =
  match (a, b) with
  | Register a, Register b -> Int32.compare a b
  | Unique a, Unique b -> Int32.compare a b
  | Register _, Unique _ -> -1
  | Unique _, Register _ -> 1

let compare_full (a : t_full) (b : t_full) =
  let c = compare a.id b.id in
  if c = 0 then
    let c = Int32.compare a.offset b.offset in
    if c = 0 then Int32.compare a.width b.width else c
  else c
