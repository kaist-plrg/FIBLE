open StdlibExt
open Notation
open Common

let rec append_build (opreands : OperandSymbol.handle_t List.t)
    (optpl : OpTpl.t) (secnum : Int32.t) : ('a, String.t) Result.t =
  let* index =
    optpl.ins |> ListExt.hd_opt
    |> Option.map (fun (x : VarNodeTpl.t) -> VarNodeTpl.get_offset x)
    |> Fun.flip Option.bind ConstTpl.try_real
    |> Option.map Int64.to_int
    |> Option.to_result ~none:"No input"
  in
  let* operand =
    List.nth_opt opreands index |> Option.to_result ~none:"No operand at index"
  in
  match operand.mapped.operand_value with
  | OTriple (Right (C c)) ->
      if Int32.compare secnum 0l >= 0 then
        let cst = Int32Map.find_opt secnum c.namedtmpl in
        match cst with
        | Some ctpl -> build_nonempty (TypeDef.C c) ctpl secnum
        | None -> build_empty (TypeDef.C c) secnum
      else
        let* ctpl =
          c.tmpl |> Option.to_result ~none:"No template for constructor"
        in
        build_nonempty (TypeDef.C c) ctpl (-1l)
  | _ -> "Build but not subtable" |> Result.error

and delay_slot (optpl : OpTpl.t) : ('a, String.t) Result.t =
  "delay_slot" |> Result.error

and set_label (optpl : OpTpl.t) : ('a, String.t) Result.t =
  "set_label" |> Result.error

and append_cross_build (optpl : OpTpl.t) (secnum : Int32.t) :
    ('a, String.t) Result.t =
  "append_cross_build" |> Result.error

and dump (optpl : OpTpl.t) : (Unit.t, String.t) Result.t =
  let* _ =
    ResultExt.fold_left_M
      (fun () (x : VarNodeTpl.t) -> () |> Result.ok)
      () (OpTpl.get_ins optpl)
  in
  () |> Result.ok

and build_nonempty (C c : Constructor.handle_t) (ctpl : ConstructTpl.t)
    (secnum : Int32.t) : (Unit.t, String.t) Result.t =
  ResultExt.fold_left_M
    (fun () (optpl : OpTpl.t) ->
      [%log info "Building op %a" OpTpl.pp_op optpl.opc];
      match OpTpl.get_opc optpl with
      | BUILD -> append_build c.operandIds optpl secnum
      | DELAY_SLOT -> delay_slot optpl
      | LABEL -> set_label optpl
      | CROSSBUILD -> append_cross_build optpl secnum
      | _ -> dump optpl)
    () ctpl.opTpls

and build_empty (C c : Constructor.handle_t) (secnum : Int32.t) :
    (Unit.t, String.t) Result.t =
  "build_empty" |> Result.error

let build (C c : Constructor.handle_t) (secnum : Int32.t) :
    (Unit.t, String.t) Result.t =
  let* ctpl = c.tmpl |> Option.to_result ~none:"No template for constructor" in
  build_nonempty (C c) ctpl secnum
