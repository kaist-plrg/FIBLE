include Stdlib.List

let take (n : int) (xs : 'a list) : 'a list =
  let rec aux (n : int) (xs : 'a list) (acc : 'a list) : 'a list =
    match (n, xs) with
    | 0, _ | _, [] -> rev acc
    | _, x :: xs -> aux (n - 1) xs (x :: acc)
  in
  aux n xs []

let hd_opt (xs : 'a list) : 'a option =
  match xs with [] -> None | x :: _ -> Some x

let t_of_sexp = Sexplib.Conv.list_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_list

let pp (pp_a : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (xs : 'a list) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
    pp_a fmt xs
