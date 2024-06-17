type 'const_t poly_t =
  | Register of RegId.t_full
  | Const of 'const_t
  | Ram of 'const_t
[@@deriving sexp]

module type S = sig
  module Const : ConstF.S

  type t = Const.t poly_t

  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val get_width : t -> Int32.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

module Make (Const : ConstF.S) = struct
  module Const = Const

  type t = Const.t poly_t [@@deriving sexp]

  let pp (fmt : Format.formatter) (v : t) =
    match v with
    | Register n -> Format.fprintf fmt "%a" RegId.pp_full n
    | Ram n -> Format.fprintf fmt "*[ram]%a" Const.pp n
    | Const n -> Format.fprintf fmt "%a" Const.pp n

  let compare = compare

  let get_width = function
    | Register n -> n.width
    | Const n -> Const.get_width n
    | Ram n -> Const.get_width n
end
