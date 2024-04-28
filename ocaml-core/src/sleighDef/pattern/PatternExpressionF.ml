open StdlibExt
open Notation

module Make (Value : sig
  type t

  val decode : Xml.xml -> SleighInit.t -> (t, String.t) Result.t
  val check_tag : String.t -> Bool.t
  val of_start : Unit.t -> t
  val of_end : Unit.t -> t
  val of_next2 : Unit.t -> t
end) =
struct
  type t = V of Value.t | Binary of Bop.t * t * t | Unary of Uop.t * t

  let rec decode (xml : Xml.xml) (sleighInit : SleighInit.t) :
      (t, String.t) Result.t =
    let tag = XmlExt.tag xml in
    let* (v : (Value.t, Bop.t, Uop.t) Either3.t) =
      match tag with
      | s when Value.check_tag s ->
          Value.decode xml sleighInit |> Result.map Either3.first
      | "plus_exp" -> Bop.Add |> Either3.second |> Result.ok
      | "sub_exp" -> Bop.Sub |> Either3.second |> Result.ok
      | "mult_exp" -> Bop.Mult |> Either3.second |> Result.ok
      | "lshift_exp" -> Bop.Lshift |> Either3.second |> Result.ok
      | "rshift_exp" -> Bop.Rshift |> Either3.second |> Result.ok
      | "and_exp" -> Bop.And |> Either3.second |> Result.ok
      | "or_exp" -> Bop.Or |> Either3.second |> Result.ok
      | "xor_exp" -> Bop.Xor |> Either3.second |> Result.ok
      | "div_exp" -> Bop.Div |> Either3.second |> Result.ok
      | "minus_exp" -> Uop.Neg |> Either3.third |> Result.ok
      | "not_exp" -> Uop.Not |> Either3.third |> Result.ok
      | _ ->
          Format.sprintf "not matched tag %s" (Xml.to_string xml)
          |> Result.error
    in
    match v with
    | Either3.First v -> V v |> Result.ok
    | Either3.Second bop ->
        let child = Xml.children xml in
        let* left, right =
          match child with
          | [ left; right ] -> (left, right) |> Result.ok
          | _ ->
              Format.sprintf "expected 2 children, got %d" (List.length child)
              |> Result.error
        in
        let* left = decode left sleighInit in
        let* right = decode right sleighInit in
        Binary (bop, left, right) |> Result.ok
    | Either3.Third uop ->
        let child = Xml.children xml in
        let* child =
          match child with
          | [ child ] -> child |> Result.ok
          | _ ->
              Format.sprintf "expected 1 child, got %d" (List.length child)
              |> Result.error
        in
        let* child = decode child sleighInit in
        Unary (uop, child) |> Result.ok

  let of_start (() : Unit.t) : t = V (Value.of_start ())
  let of_end (() : Unit.t) : t = V (Value.of_end ())
  let of_next2 (() : Unit.t) : t = V (Value.of_next2 ())

  let to_value (v : t) : Value.t Option.t =
    match v with V v -> v |> Option.some | _ -> None

  let pp (fmt : Format.formatter) (v : t) : Unit.t =
    Format.fprintf fmt "patternexp"
end
