let varnode_to_common (si : SpaceInfo.t) (rspec : RegSpec.t) (v : VarNode.t) :
    Common.VarNode.t =
  if v.space = si.unique then
    Register
      { id = Unique (Int64.to_int32 v.offset); offset = 0l; width = v.size }
  else if v.space = si.register then
    let _, base, offset =
      RegSpec.TMap.find (Int64.to_int32 v.offset, v.size) rspec.all_regs
    in
    Register { id = Register base; offset; width = v.size }
  else if v.space = si.const then Const { value = v.offset; width = v.size }
  else if v.space = si.ram then Ram { value = v.offset; width = v.size }
  else [%log fatal "Unknown space %ld" v.space]

let pcode_to_common (si : SpaceInfo.t) (rspec : RegSpec.t)
    (addr : Common.Addr.t) (seqn : Int.t) (p : PCode.t) : Common.RawInst.t_full
    =
  [%log debug "Converting %a" PCode.pp p];
  let inputs i = varnode_to_common si rspec p.inputs.(i) in
  let output_raw () =
    varnode_to_common si rspec
      ((p.output |> Option.map Fun.const
       |> Option.value ~default:(fun () ->
              [%log raise (Invalid_argument "option is None")]))
         ())
  in
  let output () =
    match output_raw () with
    | Register r -> r
    | _ -> [%log raise (Invalid_argument "Output is not a register")]
  in
  let mkJump _ =
    Common.RawInst.Ijump
      (match inputs 0 with
      | Ram { value = a; _ } -> (a, 0)
      | _ -> [%log fatal "Jump target is not a constant"])
  in
  let mkJIump _ = Common.RawInst.Ijump_ind (inputs 0) in
  let mkUop op =
    Common.RawInst.IA { expr = Auop (op, inputs 0); output = output () }
  in
  let mkBop op =
    Common.RawInst.IA
      { expr = Abop (op, inputs 0, inputs 1); output = output () }
  in
  let (inst : Common.RawInst.t) =
    match p.opcode with
    | 0l -> Iunimplemented
    | 1l -> (
        match output_raw () with
        | Register r -> IA { expr = Avar (inputs 0); output = r }
        | Ram { value = a; width = w } ->
            ILS
              (Store
                 {
                   space = Const { value = Int64.of_int32 si.ram; width = 8l };
                   pointer = Const { value = a; width = 8l };
                   value = inputs 0;
                 })
        | _ -> [%log fatal "Output is not a register or ram"])
    | 2l ->
        ILS (Load { space = inputs 0; pointer = inputs 1; output = output () })
    | 3l ->
        ILS (Store { space = inputs 0; pointer = inputs 1; value = inputs 2 })
    | 4l -> mkJump ()
    | 5l ->
        Icbranch
          {
            condition = inputs 1;
            target =
              (match inputs 0 with
              | Ram { value = a; _ } -> (a, 0)
              | _ -> [%log fatal "Jump target is not a constant"]);
          }
    | 6l -> mkJIump ()
    | 7l -> mkJump ()
    | 8l -> mkJIump ()
    | 9l -> Iunimplemented
    | 10l -> mkJIump ()
    | 11l -> mkBop Bint_equal
    | 12l -> mkBop Bint_notequal
    | 13l -> mkBop Bint_sless
    | 14l -> mkBop Bint_slessequal
    | 15l -> mkBop Bint_less
    | 16l -> mkBop Bint_lessequal
    | 17l -> mkUop Uint_zext
    | 18l -> mkUop Uint_sext
    | 19l -> mkBop Bint_add
    | 20l -> mkBop Bint_sub
    | 21l -> mkBop Bint_carry
    | 22l -> mkBop Bint_scarry
    | 23l -> mkBop Bint_sborrow
    | 24l -> mkUop Uint_2comp
    | 25l -> mkUop Uint_negate
    | 26l -> mkBop Bint_xor
    | 27l -> mkBop Bint_and
    | 28l -> mkBop Bint_or
    | 29l -> mkBop Bint_left
    | 30l -> mkBop Bint_right
    | 31l -> mkBop Bint_sright
    | 32l -> mkBop Bint_mult
    | 33l -> mkBop Bint_div
    | 34l -> mkBop Bint_sdiv
    | 35l -> mkBop Bint_rem
    | 36l -> mkBop Bint_srem
    | 37l -> mkUop Ubool_negate
    | 38l -> mkBop Bbool_xor
    | 39l -> mkBop Bbool_and
    | 40l -> mkBop Bbool_or
    | 41l -> mkBop Bfloat_equal
    | 42l -> mkBop Bfloat_notequal
    | 43l -> mkBop Bfloat_less
    | 44l -> mkBop Bfloat_lessequal
    | 46l -> mkUop Ufloat_nan
    | 47l -> mkBop Bfloat_add
    | 48l -> mkBop Bfloat_div
    | 49l -> mkBop Bfloat_mult
    | 50l -> mkBop Bfloat_sub
    | 51l -> mkUop Ufloat_neg
    | 52l -> mkUop Ufloat_abs
    | 53l -> mkUop Ufloat_sqrt
    | 54l -> mkUop Uint2float
    | 55l -> mkUop Ufloat2float
    | 57l -> mkUop Ufloat_ceil
    | 58l -> mkUop Ufloat_floor
    | 59l -> mkUop Ufloat_round
    | 62l -> mkBop Bpiece
    | 63l -> mkBop Bsubpiece
    | 72l -> mkUop Upopcount
    | 73l -> mkUop Ulzcount
    | 999l -> IN INop
    | _ -> Iunimplemented
  in
  { ins = inst; mnem = p.mnemonic; loc = (addr, seqn) }
