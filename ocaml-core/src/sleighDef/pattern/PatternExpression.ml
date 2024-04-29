open StdlibExt
open Notation
include PatternExpressionF.Make (PatternValue)

let get_value (v : t) (walker : ParserWalker.t) : (Int64.t, String.t) Result.t =
  match v with
  | V v -> PatternValue.get_value v walker
  | _ -> "TODO: Undefined" |> Result.error

let get_value_string (v : t) (walker : ParserWalker.t) :
    (String.t, String.t) Result.t =
  let* v = get_value v walker in
  if Int64.compare v Int64.zero >= 0 then Printf.sprintf "0x%Lx" v |> Result.ok
  else Printf.sprintf "-0x%Lx" (Int64.neg v) |> Result.ok
