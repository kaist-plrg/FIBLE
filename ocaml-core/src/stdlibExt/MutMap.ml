module Make (Ord : Map.OrderedType) = struct
  module Inner = Hashtbl.Make (struct
    include Ord

    let equal a b = Ord.compare a b = 0
    let hash = Hashtbl.hash
  end)

  type key = Ord.t
  type 'a t = 'a Inner.t

  let create_empty (_ : unit) : 'a t = Inner.create 16
  let is_empty (m : 'a t) : bool = Inner.length m = 0
  let mem (k : key) (m : 'a t) : bool = Inner.mem m k
  let add (k : key) (v : 'a) (m : 'a t) : unit = Inner.replace m k v

  let update (k : key) (f : 'a option -> 'a option) (h : 'a t) : unit =
    match Inner.find_opt h k with
    | Some v -> f (Some v) |> Option.iter (fun v -> Inner.replace h k v)
    | None -> f None |> Option.iter (fun v -> Inner.replace h k v)

  let singleton (k : key) (v : 'a) : 'a t =
    let h = Inner.create 16 in
    Inner.add h k v;
    h

  let remove (k : key) (m : 'a t) : unit = Inner.remove m k

  let merge (f : key -> 'a option -> 'b option -> 'a option) (m1 : 'a t)
      (m2 : 'b t) : unit =
    Inner.iter
      (fun k v2 ->
        match f k (Inner.find_opt m1 k) (Some v2) with
        | Some v -> Inner.replace m1 k v
        | None -> Inner.remove m1 k)
      m2;
    Inner.filter_map_inplace
      (fun k v ->
        match Inner.find_opt m2 k with
        | Some _ -> Some v
        | None -> (
            match f k (Some v) None with Some v -> Some v | None -> None))
      m1

  let union (f : key -> 'a -> 'a -> 'a option) (m1 : 'a t) (m2 : 'a t) : unit =
    Inner.iter
      (fun k v2 ->
        match Inner.find_opt m1 k with
        | Some v1 -> (
            match f k v1 v2 with Some v -> Inner.replace m1 k v | None -> ())
        | None -> Inner.replace m2 k v2)
      m2

  let equal (cmp : 'a -> 'a -> bool) (m1 : 'a t) (m2 : 'a t) : bool =
    Inner.length m1 = Inner.length m2
    && Inner.fold
         (fun k v acc ->
           acc
           &&
           match Inner.find_opt m2 k with Some v2 -> cmp v v2 | None -> false)
         m1 true

  let iter (f : key -> 'a -> unit) (m : 'a t) : unit = Inner.iter f m

  let fold (f : key -> 'a -> 'b -> 'b) (m : 'a t) (b : 'b) : 'b =
    Inner.fold f m b

  let for_all (f : key -> 'a -> bool) (m : 'a t) : bool =
    Inner.fold (fun k v acc -> acc && f k v) m true

  let exists (f : key -> 'a -> bool) (m : 'a t) : bool =
    Inner.fold (fun k v acc -> acc || f k v) m false

  let filter (f : key -> 'a -> bool) (m : 'a t) : 'a t =
    let h = Inner.create (Inner.length m) in
    Inner.iter (fun k v -> if f k v then Inner.replace h k v) m;
    h

  let filter_inplace (f : key -> 'a -> bool) (m : 'a t) : unit =
    Inner.filter_map_inplace (fun k v -> if f k v then Some v else None) m

  let filter_map (f : key -> 'a -> 'b option) (m : 'a t) : 'b t =
    let h = Inner.create (Inner.length m) in
    Inner.iter
      (fun k v -> match f k v with Some v -> Inner.replace h k v | None -> ())
      m;
    h

  let filter_map_inplace (f : key -> 'a -> 'a option) (m : 'a t) : unit =
    Inner.filter_map_inplace f m

  let partition (f : key -> 'a -> bool) (m : 'a t) : 'a t * 'a t =
    let h1 = Inner.create (Inner.length m) in
    let h2 = Inner.create (Inner.length m) in
    Inner.iter
      (fun k v -> if f k v then Inner.replace h1 k v else Inner.replace h2 k v)
      m;
    (h1, h2)

  let cardinal (m : 'a t) : int = Inner.length m
  let bindings (m : 'a t) : (key * 'a) list = Inner.to_seq m |> List.of_seq

  let min_binding (m : 'a t) : key * 'a =
    match
      Inner.fold
        (fun k v x ->
          match x with
          | Some (k', v') ->
              if Ord.compare k k' < 0 then Some (k, v) else Some (k', v')
          | None -> Some (k, v))
        m None
    with
    | Some x -> x
    | None -> raise Not_found

  let min_binding_opt (m : 'a t) : (key * 'a) option =
    Inner.fold
      (fun k v x ->
        match x with
        | Some (k', v') ->
            if Ord.compare k k' < 0 then Some (k, v) else Some (k', v')
        | None -> Some (k, v))
      m None

  let max_binding (m : 'a t) : key * 'a =
    match
      Inner.fold
        (fun k v x ->
          match x with
          | Some (k', v') ->
              if Ord.compare k k' > 0 then Some (k, v) else Some (k', v')
          | None -> Some (k, v))
        m None
    with
    | Some x -> x
    | None -> raise Not_found

  let max_binding_opt (m : 'a t) : (key * 'a) option =
    Inner.fold
      (fun k v x ->
        match x with
        | Some (k', v') ->
            if Ord.compare k k' > 0 then Some (k, v) else Some (k', v')
        | None -> Some (k, v))
      m None

  let choose (m : 'a t) : key * 'a =
    Inner.to_seq m |> Seq.uncons
    |> Option.value
         ~default:
           (Logger.raise __FILE__ __LINE__ (Invalid_argument "option is None"))
    |> fst

  let choose_opt (m : 'a t) : (key * 'a) option =
    Inner.to_seq m |> Seq.uncons |> Option.map fst

  let find (k : key) (m : 'a t) : 'a = Inner.find m k
  let find_opt (k : key) (m : 'a t) : 'a option = Inner.find_opt m k

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    let h = Inner.create (Inner.length m) in
    Inner.iter (fun k v -> Inner.replace h k (f v)) m;
    h

  let map_inplace (f : 'a -> 'a) (m : 'a t) : unit =
    Inner.filter_map_inplace (fun k v -> Some (f v)) m

  let mapi (f : key -> 'a -> 'b) (m : 'a t) : 'b t =
    let h = Inner.create (Inner.length m) in
    Inner.iter (fun k v -> Inner.replace h k (f k v)) m;
    h

  let map_inplace (f : key -> 'a -> 'a) (m : 'a t) : unit =
    Inner.filter_map_inplace (fun k v -> Some (f k v)) m

  let to_seq (m : 'a t) : (key * 'a) Seq.t = Inner.to_seq m
  let add_seq (s : (key * 'a) Seq.t) (m : 'a t) : unit = Inner.add_seq m s
  let of_seq (s : (key * 'a) Seq.t) : 'a t = Inner.of_seq s
end
