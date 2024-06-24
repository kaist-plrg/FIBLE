type t = Int64Set.t

let top = Int64Set.of_list [ 0L; 1L; 2L; 3L; 4L; 5L; 6L; 7L ]
let join = Int64Set.union
let meet = Int64Set.inter
let le = Int64Set.subset

let pp fmt a =
  Format.fprintf fmt "{ %a }"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt a -> Format.fprintf fmt "%Lx" a))
    (Int64Set.elements a)

let of_const c = Int64Set.singleton (Int64.unsigned_rem c 8L)

let mul (a : t) (b : t) : t =
  Int64Set.of_seq
    (Seq.flat_map
       (fun x ->
         Seq.map
           (fun y -> Int64.unsigned_rem (Int64.mul x y) 8L)
           (Int64Set.to_seq b))
       (Int64Set.to_seq a))

let add (a : t) (b : t) : t =
  Int64Set.of_seq
    (Seq.flat_map
       (fun x ->
         Seq.map
           (fun y -> Int64.unsigned_rem (Int64.add x y) 8L)
           (Int64Set.to_seq b))
       (Int64Set.to_seq a))

let sext a _ _ = a
let contains (a : t) (b : int64) = Int64Set.mem (Int64.unsigned_rem b 8L) a
