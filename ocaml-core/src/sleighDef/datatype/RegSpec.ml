module TMap = Map.Make (struct
  type t = Int32.t * Int32.t

  let compare (a1, a2) (b1, b2) =
    let c = Int32.compare a1 b1 in
    if c <> 0 then c else Int32.compare a2 b2
end)

type t = {
  base_size : Int32.t Int32Map.t;
  all_regs : (String.t * Int32.t * Int32.t) TMap.t;
}
