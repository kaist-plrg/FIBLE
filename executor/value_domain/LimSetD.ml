open StdlibExt;;

type t =
 | Top
 | LimSet of Int64Set.t

let top = Top
let limiting_carindal = 20

let lift_set c = if (Int64Set.cardinal c) > limiting_carindal then Top else LimSet c

let join a b = match (a, b) with
 | Top, _ -> Top
 | _, Top -> Top
 | LimSet a, LimSet b ->
  lift_set (Int64Set.union a b)
let meet a b = match (a, b) with
 | Top, a -> a
 | a, Top -> a
 | LimSet a, LimSet b ->
  lift_set (Int64Set.inter a b)

let ole a b = match (a, b) with
 | _, Top -> true
 | Top, _ -> false
 | LimSet a, LimSet b ->
  Int64Set.subset a b

let pp fmt a = match a with
  | Top -> Format.fprintf fmt "Top"
  | LimSet a ->
  Format.fprintf fmt "{ %a }" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (fun fmt a -> Format.fprintf fmt "%Lx" a)) (Int64Set.elements a)


let of_const c = LimSet (Int64Set.singleton ( c ))

let mul (a: t) (b: t) (width: int32): t = match (a, b) with
  | Top, _ -> Top
  | _, Top -> Top
  | LimSet a, LimSet b -> Int64Set.of_seq (
    Seq.flat_map (fun x ->
        Seq.map (fun y -> Int64Ext.cut_width (Int64.mul x y) width) (Int64Set.to_seq b)
    ) (Int64Set.to_seq a)
) |> lift_set

let add (a: t) (b: t) (width: int32): t = match (a, b) with
| Top, _ -> Top
| _, Top -> Top
| LimSet a, LimSet b -> Int64Set.of_seq (
    Seq.flat_map (fun x ->
        Seq.map (fun y -> Int64Ext.cut_width (Int64.add x y) width) (Int64Set.to_seq b)
    ) (Int64Set.to_seq a)
) |> lift_set

let sext (a: t) (in_width: int32) (out_width: int32): t = match a with
  | Top -> Top
  | LimSet a -> Int64Set.of_seq (
    Seq.map (fun x -> Int64Ext.sext x in_width out_width) (Int64Set.to_seq a)
  ) |> lift_set


let try_concretize (x: t) (limit: int): Int64Set.t option = match x with
  | Top -> None
  | LimSet a -> if (Int64Set.cardinal a) > limit then None else
    Some a