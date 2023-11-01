module Make (Ord : Set.OrderedType) = struct
  module Inner = Hashtbl.Make (struct
    include Ord

    let equal a b = Ord.compare a b = 0
    let hash = Hashtbl.hash
  end)

  type elt = Ord.t
  type t = unit Inner.t

  let create_empty (_ : unit) : t = Inner.create 16
  let is_empty (s : t) : bool = Inner.length s = 0
  let mem (e : elt) (s : t) : bool = Inner.mem s e
  let add (e : elt) (s : t) : unit = Inner.replace s e ()

  let singleton (e : elt) : t =
    let h = Inner.create 16 in
    Inner.add h e ();
    h

  let remove (e : elt) (s : t) : unit = Inner.remove s e

  let union (s1 : t) (s2 : t) : unit =
    Inner.iter (fun k v2 -> Inner.replace s1 k ()) s2

  let inter (s1 : t) (s2 : t) : unit =
    Inner.filter_map_inplace
      (fun k v -> if Inner.mem s2 k then Some () else None)
      s1

  let disjoint (s1 : t) (s2 : t) : bool =
    Inner.fold (fun k v acc -> acc && not (Inner.mem s2 k)) s1 true

  let diff (s1 : t) (s2 : t) : unit =
    Inner.filter_map_inplace
      (fun k v -> if Inner.mem s2 k then None else Some ())
      s1

  let equal (s1 : t) (s2 : t) : bool =
    Inner.length s1 = Inner.length s2
    && Inner.fold (fun k v acc -> acc && Inner.mem s2 k) s1 true

  let subset (s1 : t) (s2 : t) : bool =
    Inner.fold (fun k v acc -> acc && Inner.mem s2 k) s1 true

  let iter (f : elt -> unit) (s : t) : unit = Inner.iter (fun k _ -> f k) s

  let map (f : elt -> elt) (s : t) : t =
    let h = Inner.create (Inner.length s) in
    Inner.iter (fun k _ -> Inner.replace h (f k) ()) s;
    h

  let fold (f : elt -> 'a -> 'a) (s : t) (b : 'a) : 'a =
    Inner.fold (fun k _ acc -> f k acc) s b

  let for_all (f : elt -> bool) (s : t) : bool =
    Inner.fold (fun k _ acc -> acc && f k) s true

  let exists (f : elt -> bool) (s : t) : bool =
    Inner.fold (fun k _ acc -> acc || f k) s false

  let filter (f : elt -> bool) (s : t) : t =
    let h = Inner.create (Inner.length s) in
    Inner.iter (fun k _ -> if f k then Inner.replace h k ()) s;
    h

  let filter_inplace (f : elt -> bool) (s : t) : unit =
    Inner.filter_map_inplace (fun k _ -> if f k then Some () else None) s

  let filter_map (f : elt -> elt option) (s : t) : t =
    let h = Inner.create (Inner.length s) in
    Inner.iter
      (fun k _ ->
        match f k with Some k' -> Inner.replace h k' () | None -> ())
      s;
    h

  let partition (f : elt -> bool) (s : t) : t * t =
    let h1 = Inner.create (Inner.length s) in
    let h2 = Inner.create (Inner.length s) in
    Inner.iter
      (fun k _ -> if f k then Inner.replace h1 k () else Inner.replace h2 k ())
      s;
    (h1, h2)

  let cardinal (s : t) : int = Inner.length s
  let elements (s : t) : elt list = Inner.to_seq s |> Seq.map fst |> List.of_seq

  let min_elt (s : t) : elt =
    match
      Inner.fold
        (fun k _ x ->
          match x with
          | Some k' -> if Ord.compare k k' < 0 then Some k else Some k'
          | None -> Some k)
        s None
    with
    | Some x -> x
    | None -> raise Not_found

  let min_elt_opt (s : t) : elt option =
    Inner.fold
      (fun k _ x ->
        match x with
        | Some k' -> if Ord.compare k k' < 0 then Some k else Some k'
        | None -> Some k)
      s None

  let max_elt (s : t) : elt =
    match
      Inner.fold
        (fun k _ x ->
          match x with
          | Some k' -> if Ord.compare k k' > 0 then Some k else Some k'
          | None -> Some k)
        s None
    with
    | Some x -> x
    | None -> raise Not_found

  let max_elt_opt (s : t) : elt option =
    Inner.fold
      (fun k _ x ->
        match x with
        | Some k' -> if Ord.compare k k' > 0 then Some k else Some k'
        | None -> Some k)
      s None

  let choose (s : t) : elt =
    Inner.to_seq s |> Seq.uncons
    |> Option.value
         ~default:
           (Logger.raise __FILE__ __LINE__ (Invalid_argument "option is None"))
    |> fst |> fst

  let choose_opt (s : t) : elt option =
    Inner.to_seq s |> Seq.uncons |> Option.map (fun k -> fst (fst k))

  let find (e : elt) (s : t) : elt =
    if Inner.mem s e then e else raise Not_found

  let find_opt (e : elt) (s : t) : elt option =
    if Inner.mem s e then Some e else None

  let of_list (es : elt list) : t =
    let h = Inner.create (List.length es) in
    List.iter (fun e -> Inner.replace h e ()) es;
    h

  let to_seq (s : t) : elt Seq.t = Inner.to_seq s |> Seq.map fst

  let add_seq (es : elt Seq.t) (s : t) : unit =
    Seq.iter (fun e -> Inner.replace s e ()) es

  let of_seq (es : elt Seq.t) : t = Inner.of_seq (Seq.map (fun e -> (e, ())) es)
end
