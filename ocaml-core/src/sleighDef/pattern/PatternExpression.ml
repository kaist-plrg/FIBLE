include PatternExpressionF.Make (PatternValue)

let rec get_value (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (Int64.t, String.t) Result.t =
  match v with
  | V v -> PatternValue.get_value v walker pinfo
  | Binary (op, el, er) ->
      let* l = get_value el walker pinfo in
      let* r = get_value er walker pinfo in
      let* op =
        match op with
        | Add -> Int64.add |> Result.ok
        | Sub -> Int64.sub |> Result.ok
        | Mult -> Int64.mul |> Result.ok
        | Lshift ->
            (fun l r -> Int64.shift_left l (Int64.to_int r)) |> Result.ok
        | Rshift ->
            (fun l r -> Int64.shift_right_logical l (Int64.to_int r))
            |> Result.ok
        | And -> Int64.logand |> Result.ok
        | Or -> Int64.logor |> Result.ok
        | Xor -> Int64.logxor |> Result.ok
        | Div ->
            if Int64.equal r 0L then "Div by 0" |> Result.error
            else Int64.div |> Result.ok
      in
      op l r |> Result.ok
  | Unary (op, e) -> (
      let* v = get_value e walker pinfo in
      match op with
      | Neg -> Int64.neg v |> Result.ok
      | Not -> Int64.lognot v |> Result.ok)

let get_value_string (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (String.t, String.t) Result.t =
  let* v = get_value v walker pinfo in
  if Int64.compare v Int64.zero >= 0 then Printf.sprintf "0x%Lx" v |> Result.ok
  else Printf.sprintf "-0x%Lx" (Int64.neg v) |> Result.ok
