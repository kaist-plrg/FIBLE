open StdlibExt
open Notation

type op_t =
  | BLANK
  | COPY
  | LOAD
  | STORE
  | BRANCH
  | CBRANCH
  | BRANCHIND
  | CALL
  | CALLIND
  | CALLOTHER
  | RETURN
  | INT_EQUAL
  | INT_NOTEQUAL
  | INT_SLESS
  | INT_SLESSEQUAL
  | INT_LESS
  | INT_LESSEQUAL
  | INT_ZEXT
  | INT_SEXT
  | INT_ADD
  | INT_SUB
  | INT_CARRY
  | INT_SCARRY
  | INT_SBORROW
  | INT_2COMP
  | INT_NEGATE
  | INT_XOR
  | INT_AND
  | INT_OR
  | INT_LEFT
  | INT_RIGHT
  | INT_SRIGHT
  | INT_MULT
  | INT_DIV
  | INT_SDIV
  | INT_REM
  | INT_SREM
  | BOOL_NEGATE
  | BOOL_XOR
  | BOOL_AND
  | BOOL_OR
  | FLOAT_EQUAL
  | FLOAT_NOTEQUAL
  | FLOAT_LESS
  | FLOAT_LESSEQUAL
  | UNUSED1
  | FLOAT_NAN
  | FLOAT_ADD
  | FLOAT_DIV
  | FLOAT_MULT
  | FLOAT_SUB
  | FLOAT_NEG
  | FLOAT_ABS
  | FLOAT_SQRT
  | INT2FLOAT
  | FLOAT2FLOAT
  | TRUNC
  | CEIL
  | FLOOR
  | ROUND
  | BUILD
  | DELAY_SLOT
  | PIECE
  | SUBPIECE
  | CAST
  | LABEL
  | CROSSBUILD
  | SEGMENTOP
  | CPOOLREF
  | NEW
  | INSERT
  | EXTRACT
  | POPCOUNT
  | LZCOUNT

let match_op (s : String.t) : op_t Option.t =
  match s with
  | "BLANK" -> BLANK |> Option.some
  | "COPY" -> COPY |> Option.some
  | "LOAD" -> LOAD |> Option.some
  | "STORE" -> STORE |> Option.some
  | "BRANCH" -> BRANCH |> Option.some
  | "CBRANCH" -> CBRANCH |> Option.some
  | "BRANCHIND" -> BRANCHIND |> Option.some
  | "CALL" -> CALL |> Option.some
  | "CALLIND" -> CALLIND |> Option.some
  | "CALLOTHER" -> CALLOTHER |> Option.some
  | "RETURN" -> RETURN |> Option.some
  | "INT_EQUAL" -> INT_EQUAL |> Option.some
  | "INT_NOTEQUAL" -> INT_NOTEQUAL |> Option.some
  | "INT_SLESS" -> INT_SLESS |> Option.some
  | "INT_SLESSEQUAL" -> INT_SLESSEQUAL |> Option.some
  | "INT_LESS" -> INT_LESS |> Option.some
  | "INT_LESSEQUAL" -> INT_LESSEQUAL |> Option.some
  | "INT_ZEXT" -> INT_ZEXT |> Option.some
  | "INT_SEXT" -> INT_SEXT |> Option.some
  | "INT_ADD" -> INT_ADD |> Option.some
  | "INT_SUB" -> INT_SUB |> Option.some
  | "INT_CARRY" -> INT_CARRY |> Option.some
  | "INT_SCARRY" -> INT_SCARRY |> Option.some
  | "INT_SBORROW" -> INT_SBORROW |> Option.some
  | "INT_2COMP" -> INT_2COMP |> Option.some
  | "INT_NEGATE" -> INT_NEGATE |> Option.some
  | "INT_XOR" -> INT_XOR |> Option.some
  | "INT_AND" -> INT_AND |> Option.some
  | "INT_OR" -> INT_OR |> Option.some
  | "INT_LEFT" -> INT_LEFT |> Option.some
  | "INT_RIGHT" -> INT_RIGHT |> Option.some
  | "INT_SRIGHT" -> INT_SRIGHT |> Option.some
  | "INT_MULT" -> INT_MULT |> Option.some
  | "INT_DIV" -> INT_DIV |> Option.some
  | "INT_SDIV" -> INT_SDIV |> Option.some
  | "INT_REM" -> INT_REM |> Option.some
  | "INT_SREM" -> INT_SREM |> Option.some
  | "BOOL_NEGATE" -> BOOL_NEGATE |> Option.some
  | "BOOL_XOR" -> BOOL_XOR |> Option.some
  | "BOOL_AND" -> BOOL_AND |> Option.some
  | "BOOL_OR" -> BOOL_OR |> Option.some
  | "FLOAT_EQUAL" -> FLOAT_EQUAL |> Option.some
  | "FLOAT_NOTEQUAL" -> FLOAT_NOTEQUAL |> Option.some
  | "FLOAT_LESS" -> FLOAT_LESS |> Option.some
  | "FLOAT_LESSEQUAL" -> FLOAT_LESSEQUAL |> Option.some
  | "UNUSED1" -> UNUSED1 |> Option.some
  | "FLOAT_NAN" -> FLOAT_NAN |> Option.some
  | "FLOAT_ADD" -> FLOAT_ADD |> Option.some
  | "FLOAT_DIV" -> FLOAT_DIV |> Option.some
  | "FLOAT_MULT" -> FLOAT_MULT |> Option.some
  | "FLOAT_SUB" -> FLOAT_SUB |> Option.some
  | "FLOAT_NEG" -> FLOAT_NEG |> Option.some
  | "FLOAT_ABS" -> FLOAT_ABS |> Option.some
  | "FLOAT_SQRT" -> FLOAT_SQRT |> Option.some
  | "INT2FLOAT" -> INT2FLOAT |> Option.some
  | "FLOAT2FLOAT" -> FLOAT2FLOAT |> Option.some
  | "TRUNC" -> TRUNC |> Option.some
  | "CEIL" -> CEIL |> Option.some
  | "FLOOR" -> FLOOR |> Option.some
  | "ROUND" -> ROUND |> Option.some
  | "BUILD" -> BUILD |> Option.some
  | "DELAY_SLOT" -> DELAY_SLOT |> Option.some
  | "PIECE" -> PIECE |> Option.some
  | "SUBPIECE" -> SUBPIECE |> Option.some
  | "CAST" -> CAST |> Option.some
  | "LABEL" -> LABEL |> Option.some
  | "CROSSBUILD" -> CROSSBUILD |> Option.some
  | "SEGMENTOP" -> SEGMENTOP |> Option.some
  | "CPOOLREF" -> CPOOLREF |> Option.some
  | "NEW" -> NEW |> Option.some
  | "INSERT" -> INSERT |> Option.some
  | "EXTRACT" -> EXTRACT |> Option.some
  | "POPCOUNT" -> POPCOUNT |> Option.some
  | "LZCOUNT" -> LZCOUNT |> Option.some
  | _ -> None

let opcode (op : op_t) : Int32.t =
  match op with
  | BLANK -> 0l
  | COPY -> 1l
  | LOAD -> 2l
  | STORE -> 3l
  | BRANCH -> 4l
  | CBRANCH -> 5l
  | BRANCHIND -> 6l
  | CALL -> 7l
  | CALLIND -> 8l
  | CALLOTHER -> 9l
  | RETURN -> 10l
  | INT_EQUAL -> 11l
  | INT_NOTEQUAL -> 12l
  | INT_SLESS -> 13l
  | INT_SLESSEQUAL -> 14l
  | INT_LESS -> 15l
  | INT_LESSEQUAL -> 16l
  | INT_ZEXT -> 17l
  | INT_SEXT -> 18l
  | INT_ADD -> 19l
  | INT_SUB -> 20l
  | INT_CARRY -> 21l
  | INT_SCARRY -> 22l
  | INT_SBORROW -> 23l
  | INT_2COMP -> 24l
  | INT_NEGATE -> 25l
  | INT_XOR -> 26l
  | INT_AND -> 27l
  | INT_OR -> 28l
  | INT_LEFT -> 29l
  | INT_RIGHT -> 30l
  | INT_SRIGHT -> 31l
  | INT_MULT -> 32l
  | INT_DIV -> 33l
  | INT_SDIV -> 34l
  | INT_REM -> 35l
  | INT_SREM -> 36l
  | BOOL_NEGATE -> 37l
  | BOOL_XOR -> 38l
  | BOOL_AND -> 39l
  | BOOL_OR -> 40l
  | FLOAT_EQUAL -> 41l
  | FLOAT_NOTEQUAL -> 42l
  | FLOAT_LESS -> 43l
  | FLOAT_LESSEQUAL -> 44l
  | UNUSED1 -> 45l
  | FLOAT_NAN -> 46l
  | FLOAT_ADD -> 47l
  | FLOAT_DIV -> 48l
  | FLOAT_MULT -> 49l
  | FLOAT_SUB -> 50l
  | FLOAT_NEG -> 51l
  | FLOAT_ABS -> 52l
  | FLOAT_SQRT -> 53l
  | INT2FLOAT -> 54l
  | FLOAT2FLOAT -> 55l
  | TRUNC -> 56l
  | CEIL -> 57l
  | FLOOR -> 58l
  | ROUND -> 59l
  | BUILD -> 60l
  | DELAY_SLOT -> 61l
  | PIECE -> 62l
  | SUBPIECE -> 63l
  | CAST -> 64l
  | LABEL -> 65l
  | CROSSBUILD -> 66l
  | SEGMENTOP -> 67l
  | CPOOLREF -> 68l
  | NEW -> 69l
  | INSERT -> 70l
  | EXTRACT -> 71l
  | POPCOUNT -> 72l
  | LZCOUNT -> 73l

type t = { opc : op_t; out : VarNodeTpl.t Option.t; ins : VarNodeTpl.t List.t }

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* opc = XmlExt.attrib xml "code" in
  let* opc = match_op opc |> Option.to_result ~none:"not matched op" in
  let childs = XmlExt.children xml in
  let* hd, ins =
    match childs with
    | hd :: ins -> (hd, ins) |> Result.ok
    | _ -> "not matched children" |> Result.error
  in
  let* out =
    match XmlExt.tag hd with
    | "null" -> None |> Result.ok
    | _ -> VarNodeTpl.decode hd sleighInit |> Result.map Option.some
  in
  let* ins =
    ins
    |> List.map (fun xml -> VarNodeTpl.decode xml sleighInit)
    |> ResultExt.join_list
  in
  { opc; out; ins } |> Result.ok

let get_opc { opc; _ } = opc
let get_out { out; _ } = out
let get_ins { ins; _ } = ins

let pp_op (fmt : Format.formatter) (op : op_t) : unit =
  match op with
  | BLANK -> Format.fprintf fmt "BLANK"
  | COPY -> Format.fprintf fmt "COPY"
  | LOAD -> Format.fprintf fmt "LOAD"
  | STORE -> Format.fprintf fmt "STORE"
  | BRANCH -> Format.fprintf fmt "BRANCH"
  | CBRANCH -> Format.fprintf fmt "CBRANCH"
  | BRANCHIND -> Format.fprintf fmt "BRANCHIND"
  | CALL -> Format.fprintf fmt "CALL"
  | CALLIND -> Format.fprintf fmt "CALLIND"
  | CALLOTHER -> Format.fprintf fmt "CALLOTHER"
  | RETURN -> Format.fprintf fmt "RETURN"
  | INT_EQUAL -> Format.fprintf fmt "INT_EQUAL"
  | INT_NOTEQUAL -> Format.fprintf fmt "INT_NOTEQUAL"
  | INT_SLESS -> Format.fprintf fmt "INT_SLESS"
  | INT_SLESSEQUAL -> Format.fprintf fmt "INT_SLESSEQUAL"
  | INT_LESS -> Format.fprintf fmt "INT_LESS"
  | INT_LESSEQUAL -> Format.fprintf fmt "INT_LESSEQUAL"
  | INT_ZEXT -> Format.fprintf fmt "INT_ZEXT"
  | INT_SEXT -> Format.fprintf fmt "INT_SEXT"
  | INT_ADD -> Format.fprintf fmt "INT_ADD"
  | INT_SUB -> Format.fprintf fmt "INT_SUB"
  | INT_CARRY -> Format.fprintf fmt "INT_CARRY"
  | INT_SCARRY -> Format.fprintf fmt "INT_SCARRY"
  | INT_SBORROW -> Format.fprintf fmt "INT_SBORROW"
  | INT_2COMP -> Format.fprintf fmt "INT_2COMP"
  | INT_NEGATE -> Format.fprintf fmt "INT_NEGATE"
  | INT_XOR -> Format.fprintf fmt "INT_XOR"
  | INT_AND -> Format.fprintf fmt "INT_AND"
  | INT_OR -> Format.fprintf fmt "INT_OR"
  | INT_LEFT -> Format.fprintf fmt "INT_LEFT"
  | INT_RIGHT -> Format.fprintf fmt "INT_RIGHT"
  | INT_SRIGHT -> Format.fprintf fmt "INT_SRIGHT"
  | INT_MULT -> Format.fprintf fmt "INT_MULT"
  | INT_DIV -> Format.fprintf fmt "INT_DIV"
  | INT_SDIV -> Format.fprintf fmt "INT_SDIV"
  | INT_REM -> Format.fprintf fmt "INT_REM"
  | INT_SREM -> Format.fprintf fmt "INT_SREM"
  | BOOL_NEGATE -> Format.fprintf fmt "BOOL_NEGATE"
  | BOOL_XOR -> Format.fprintf fmt "BOOL_XOR"
  | BOOL_AND -> Format.fprintf fmt "BOOL_AND"
  | BOOL_OR -> Format.fprintf fmt "BOOL_OR"
  | FLOAT_EQUAL -> Format.fprintf fmt "FLOAT_EQUAL"
  | FLOAT_NOTEQUAL -> Format.fprintf fmt "FLOAT_NOTEQUAL"
  | FLOAT_LESS -> Format.fprintf fmt "FLOAT_LESS"
  | FLOAT_LESSEQUAL -> Format.fprintf fmt "FLOAT_LESSEQUAL"
  | UNUSED1 -> Format.fprintf fmt "UNUSED1"
  | FLOAT_NAN -> Format.fprintf fmt "FLOAT_NAN"
  | FLOAT_ADD -> Format.fprintf fmt "FLOAT_ADD"
  | FLOAT_DIV -> Format.fprintf fmt "FLOAT_DIV"
  | FLOAT_MULT -> Format.fprintf fmt "FLOAT_MULT"
  | FLOAT_SUB -> Format.fprintf fmt "FLOAT_SUB"
  | FLOAT_NEG -> Format.fprintf fmt "FLOAT_NEG"
  | FLOAT_ABS -> Format.fprintf fmt "FLOAT_ABS"
  | FLOAT_SQRT -> Format.fprintf fmt "FLOAT_SQRT"
  | INT2FLOAT -> Format.fprintf fmt "INT2FLOAT"
  | FLOAT2FLOAT -> Format.fprintf fmt "FLOAT2FLOAT"
  | TRUNC -> Format.fprintf fmt "TRUNC"
  | CEIL -> Format.fprintf fmt "CEIL"
  | FLOOR -> Format.fprintf fmt "FLOOR"
  | ROUND -> Format.fprintf fmt "ROUND"
  | BUILD -> Format.fprintf fmt "BUILD"
  | DELAY_SLOT -> Format.fprintf fmt "DELAY_SLOT"
  | PIECE -> Format.fprintf fmt "PIECE"
  | SUBPIECE -> Format.fprintf fmt "SUBPIECE"
  | CAST -> Format.fprintf fmt "CAST"
  | LABEL -> Format.fprintf fmt "LABEL"
  | CROSSBUILD -> Format.fprintf fmt "CROSSBUILD"
  | SEGMENTOP -> Format.fprintf fmt "SEGMENTOP"
  | CPOOLREF -> Format.fprintf fmt "CPOOLREF"
  | NEW -> Format.fprintf fmt "NEW"
  | INSERT -> Format.fprintf fmt "INSERT"
  | EXTRACT -> Format.fprintf fmt "EXTRACT"
  | POPCOUNT -> Format.fprintf fmt "POPCOUNT"
  | LZCOUNT -> Format.fprintf fmt "LZCOUNT"
