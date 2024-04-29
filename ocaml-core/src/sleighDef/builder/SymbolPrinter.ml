open StdlibExt
open Notation

let rec drop (n : int) (l : 'a list) : 'a list =
  if n <= 0 then l else match l with [] -> [] | _ :: t -> drop (n - 1) t

let rec print_oper (o : OperandSymbol.mapped_t) (sla : Sla.t)
    (walker : ParserWalker.t) : (String.t, String.t) Result.t =
  match o.operand_value with
  | OTriple (Left a) -> (
      match a with
      | Specific (Operand a) -> print_oper o sla walker
      | Specific (End v) -> EndSymbol.print v walker
      | Specific (Start v) -> StartSymbol.print v walker
      | Specific (Next2 v) -> Next2Symbol.print v walker
      | Specific (Patternless v) -> PatternlessSymbol.print v walker
      | Family a -> FamilySymbol.print a walker)
  | OTriple (Right i) -> print_constructor i sla walker
  | ODefExp p ->
      let* pexp = Sla.translate_oe sla p in
      PatternExpression.get_value_string pexp walker

and print_constructor ({ offset; mapped = c; _ } : Constructor.mapped_t)
    (sla : Sla.t) (walker : ParserWalker.t) : (String.t, String.t) Result.t =
  let nwalker = ParserWalker.replace_offset walker offset in
  let* op_list =
    List.map
      (fun (p : TypeDef.printpiece) ->
        match p with
        | Str s -> s |> Result.ok
        | OperInd op ->
            let* op =
              List.nth_opt c.operandIds (Int32.to_int op)
              |> Option.to_result ~none:"OperInd out of bounds"
            in
            print_oper op sla nwalker)
      c.printpieces
    |> ResultExt.join_list
  in
  Ok (String.concat "" op_list)

let print_body ({ mapped = s; offset } : Constructor.mapped_t) (sla : Sla.t)
    (walker : ParserWalker.t) : (String.t, String.t) Result.t =
  let nwalker = ParserWalker.replace_offset walker offset in
  match s.flowthruIndex with
  | Some i -> Ok (Printf.sprintf "%ld" i)
  | None -> (
      match s.firstWhitespace with
      | -1l -> "" |> Result.ok
      | i ->
          let i = Int32.to_int i in
          let d = drop i s.printpieces in
          let* op_list =
            List.map
              (fun (p : TypeDef.printpiece) ->
                match p with
                | Str s -> s |> Result.ok
                | OperInd op ->
                    let* op =
                      List.nth_opt s.operandIds (Int32.to_int op)
                      |> Option.to_result ~none:"OperInd out of bounds"
                    in
                    print_oper op sla nwalker)
              d
            |> ResultExt.join_list
          in
          Ok (String.concat "" op_list))
