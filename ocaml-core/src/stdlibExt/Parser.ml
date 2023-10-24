type 'a t = String.t -> 'a option
type 'a t_total = String.t -> 'a

let from_format
    (format :
      ( 'a,
        Scanf.Scanning.in_channel,
        'b,
        'c,
        'a -> 'd option,
        'd )
      Stdlib.format6) (f : 'c -> 'd option) : 'd t =
 fun s -> f (Scanf.sscanf_opt s (format ^^ "%!"))

let from_list (l : 'd t list) (default : 'd) : 'd t_total =
 fun s ->
  let rec aux l =
    match l with
    | [] -> default
    | f :: l -> ( match f s with Some x -> x | None -> aux l)
  in
  aux l
