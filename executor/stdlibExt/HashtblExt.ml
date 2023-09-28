include Hashtbl

module Make (H: Hashtbl.HashedType) = struct
  include Hashtbl.Make(H)
  let update (h: 'a t) (k: H.t) (f: 'a option -> 'a option): unit =
    match find_opt h k with
    | Some v -> f (Some v) |> Option.iter (fun v -> replace h k v)
    | None -> f None |> Option.iter (fun v -> add h k v)
end