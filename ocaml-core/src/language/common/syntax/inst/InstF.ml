open StdlibExt

module Make2 (Inst1 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst2 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) =
struct
  type t = (Inst1.t, Inst2.t) Either.t

  let first (a : Inst1.t) : t = Either.left a
  let second (a : Inst2.t) : t = Either.right a

  let fold (first : Inst1.t -> 'a) (second : Inst2.t -> 'a) =
    Either.fold ~left:first ~right:second

  let pp fmt = fold (Inst1.pp fmt) (Inst2.pp fmt)
  let is_nop = fold Inst1.is_nop Inst2.is_nop
end

module Make3 (Inst1 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst2 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst3 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) =
struct
  type t = (Inst1.t, Inst2.t, Inst3.t) Either3.t

  let first (a : Inst1.t) : t = Either3.first a
  let second (a : Inst2.t) : t = Either3.second a
  let third (a : Inst3.t) : t = Either3.third a

  let fold (first : Inst1.t -> 'a) (second : Inst2.t -> 'a)
      (third : Inst3.t -> 'a) =
    Either3.fold first second third

  let pp fmt = fold (Inst1.pp fmt) (Inst2.pp fmt) (Inst3.pp fmt)
  let is_nop = fold Inst1.is_nop Inst2.is_nop Inst3.is_nop
end

module Make4 (Inst1 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst2 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst3 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst4 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) =
struct
  type t = (Inst1.t, Inst2.t, Inst3.t, Inst4.t) Either4.t

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

module Make7 (Inst1 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst2 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst3 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst4 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst5 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst6 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) (Inst7 : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
end) =
struct
  type t =
    (Inst1.t, Inst2.t, Inst3.t, Inst4.t, Inst5.t, Inst6.t, Inst7.t) Either7.t

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
