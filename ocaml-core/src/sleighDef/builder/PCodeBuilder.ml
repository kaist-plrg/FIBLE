let rec append_build (opreands : OperandSymbol.handle_t List.t)
    (optpl : OpTpl.t) (secnum : Int32.t) (walker : ParserWalker.t)
    (pinfo : PatternInfo.t) : ('a, String.t) Result.t =
  let* index =
    optpl.ins |> List.hd_opt
    |> Option.map (fun (x : VarNodeTpl.t) -> VarNodeTpl.get_offset x)
    |> Fun.flip Option.bind ConstTpl.try_real
    |> Option.map Int64.to_int
    |> Option.to_result ~none:"No input"
  in
  let* operand =
    List.nth_opt opreands index |> Option.to_result ~none:"No operand at index"
  in
  let nwalker = ParserWalker.replace_offset walker operand.mapped.offset in
  match operand.operand_value with
  | OTriple (Right (C c)) ->
      if Int32.compare secnum 0l >= 0 then
        let cst = Int32Map.find_opt secnum c.namedtmpl in
        match cst with
        | Some ctpl -> build_nonempty (TypeDef.C c) ctpl secnum nwalker pinfo
        | None -> build_empty (TypeDef.C c) secnum
      else
        let* ctpl =
          c.tmpl |> Option.to_result ~none:"No template for constructor"
        in
        build_nonempty (TypeDef.C c) ctpl (-1l) nwalker pinfo
  | _ -> "Build but not subtable" |> Result.error

and delay_slot (optpl : OpTpl.t) : ('a, String.t) Result.t =
  "delay_slot" |> Result.error

and set_label (optpl : OpTpl.t) : ('a, String.t) Result.t =
  "set_label" |> Result.error

and append_cross_build (optpl : OpTpl.t) (secnum : Int32.t) :
    ('a, String.t) Result.t =
  "append_cross_build" |> Result.error

and dump (optpl : OpTpl.t) (operands : FixedHandle.t List.t)
    (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (PCode.core_t List.t, String.t) Result.t =
  let* pre_insts, inputs =
    Result.fold_left_M
      (fun (l, i) (x : VarNodeTpl.t) ->
        let* b = VarNodeTpl.is_dynamic x operands walker in
        let* nl, ni =
          if b then
            let* handl =
              x.offset |> ConstTpl.try_handle
              |> Option.to_result ~none:"No handle"
            in
            let* v = VarNodeTpl.generateLocation x operands pinfo in
            let* pt, spc = VarNodeTpl.generatePointer x operands pinfo in
            let load_op =
              PCode.make (OpTpl.opcode OpTpl.LOAD)
                [|
                  VarNode.make
                    (AddrSpace.const_space |> AddrSpace.get_index
                   |> Int32.of_int)
                    8l
                    (AddrSpace.get_index spc |> Int64.of_int);
                  pt;
                |]
                (Some v)
            in
            match snd handl with
            | OffsetPlus _ -> "unimpl_offset_plus" |> Result.error
            | _ -> ([ load_op ], v) |> Result.ok
          else
            let* v = VarNodeTpl.generateLocation x operands pinfo in
            ([], v) |> Result.ok
        in
        (List.append l nl, ni :: i) |> Result.ok)
      ([], []) (OpTpl.get_ins optpl)
  in
  let pre_insts, inputs = (pre_insts, List.rev inputs |> Array.of_list) in
  let* post_insts, output =
    OpTpl.get_out optpl
    |> Option.map (fun (x : VarNodeTpl.t) ->
           let* b = VarNodeTpl.is_dynamic x operands walker in
           if b then
             let* handl =
               x.offset |> ConstTpl.try_handle
               |> Option.to_result ~none:"No handle"
             in
             let* v = VarNodeTpl.generateLocation x operands pinfo in
             let* pt, spc = VarNodeTpl.generatePointer x operands pinfo in
             let store_op =
               PCode.make (OpTpl.opcode OpTpl.STORE)
                 [|
                   VarNode.make
                     (AddrSpace.const_space |> AddrSpace.get_index
                    |> Int32.of_int)
                     8l
                     (AddrSpace.get_index spc |> Int64.of_int);
                   pt;
                   v;
                 |]
                 None
             in
             match snd handl with
             | OffsetPlus _ -> "unimpl_offset_plus" |> Result.error
             | _ -> ([ store_op ], Some v) |> Result.ok
           else
             let* v = VarNodeTpl.generateLocation x operands pinfo in
             ([], Some v) |> Result.ok)
    |> Option.value ~default:(([], None) |> Result.ok)
  in
  let cur_pcode = PCode.make (OpTpl.opcode optpl.opc) inputs output in
  List.append pre_insts (cur_pcode :: post_insts) |> Result.ok

and build_nonempty (C c : Constructor.handle_t) (ctpl : ConstructTpl.t)
    (secnum : Int32.t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (PCode.core_t List.t, String.t) Result.t =
  Result.fold_left_M
    (fun nplist (optpl : OpTpl.t) ->
      let* plist =
        match OpTpl.get_opc optpl with
        | BUILD -> append_build c.operandIds optpl secnum walker pinfo
        | DELAY_SLOT -> delay_slot optpl
        | LABEL -> set_label optpl
        | CROSSBUILD -> append_cross_build optpl secnum
        | _ ->
            [%log debug "Dumping op %a" OpTpl.pp_op optpl.opc];
            dump optpl
              (c.operandIds
              |> List.map (fun (x : OperandSymbol.handle_t) -> x.mapped.handle)
              )
              walker pinfo
      in
      List.append nplist plist |> Result.ok)
    [] ctpl.opTpls

and build_empty (C c : Constructor.handle_t) (secnum : Int32.t) :
    (PCode.core_t List.t, String.t) Result.t =
  "build_empty" |> Result.error

let build (C c : Constructor.handle_t) (secnum : Int32.t)
    (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (PCode.core_t List.t, String.t) Result.t =
  let* ctpl = c.tmpl |> Option.to_result ~none:"No template for constructor" in
  build_nonempty (C c) ctpl secnum walker pinfo

let build_with_mnemonic (C c : Constructor.handle_t) (secnum : Int32.t)
    (walker : ParserWalker.t) (pinfo : PatternInfo.t) (mnemonic : String.t) :
    (PCode.t List.t, String.t) Result.t =
  let* ctpl = c.tmpl |> Option.to_result ~none:"No template for constructor" in
  let* codes = build_nonempty (C c) ctpl secnum walker pinfo in
  List.map (fun x -> PCode.append_mnemonic x mnemonic) codes |> Result.ok
