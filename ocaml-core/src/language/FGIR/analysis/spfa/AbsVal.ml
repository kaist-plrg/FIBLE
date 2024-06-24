open Common
open Basic_domain

module Inner = struct
  type t = SP of Int64.t | Const of NumericValue.t | RVal

  let pp fmt x =
    match x with
    | SP x -> Format.fprintf fmt "SP %Ld" x
    | Const x -> Format.fprintf fmt "Const %a" NumericValue.pp x
    | RVal -> Format.fprintf fmt "RVal"

  let eval_bop (bop : Bop.t) x y width : t Option.t =
    match (bop, x, y) with
    | _, Const x, Const y ->
        NumericBop.eval bop x y width
        |> Result.to_option
        |> Option.map (fun x -> Const x)
    | Bop.Bint_add, SP x, Const y | Bop.Bint_add, Const y, SP x ->
        Some (SP (Int64.add x (NumericValue.value_64 y |> Result.get_ok)))
    | Bop.Bint_sub, SP x, Const y ->
        Some (SP (Int64.sub x (NumericValue.value_64 y |> Result.get_ok)))
    | Bop.Bint_sub, SP x, SP y ->
        Some (Const (NumericValue.of_int64 (Int64.sub x y) 8l))
    | _ -> None

  let eval_uop (uop : Uop.t) x width : t Option.t =
    match (uop, x) with
    | _, Const x ->
        NumericUop.eval uop x width
        |> Result.to_option
        |> Option.map (fun x -> Const x)
    | _ -> None
end

include Inner

type elem_t = t

include FlatD.MakeValue (Inner)

let bottom = bot
let of_const (x : NumericValue.t) : t = Flat (Const x)
let of_sp_offset (x : Int64.t) = Flat (SP x)
let of_rval = Flat RVal
