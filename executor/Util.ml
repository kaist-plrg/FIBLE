open PCode;;


type spaceinfo = {
  unique: int32;
  register: int32;
  const: int32
};;

type varnode_raw = {
  space: int32;
  offset: int64;
  size: int32;
};;

type pcode_raw = {
  opcode: int32;
  inputs: varnode_raw array;
  output: varnode_raw option;
};;


let string_of_varnode (v: varNode) =
  match v.varNode_node with
  | Register n -> Printf.sprintf "$%Ld:%ld" n v.varNode_width
  | Unique n -> Printf.sprintf "#%Ld:%ld" n v.varNode_width
  | Const n -> Printf.sprintf "%Ld:%ld" n v.varNode_width
;;

let string_of_assignable (a: assignable) =
  match a with
   | Avar (vn) -> string_of_varnode vn
   | Auop (op, vn) -> (match op with
    | Upopcount -> Printf.sprintf "popcount(%s)" (string_of_varnode vn)
    | Ulzcount -> Printf.sprintf "lzcount(%s)" (string_of_varnode vn)
    | Uint_zext -> Printf.sprintf "zext(%s)" (string_of_varnode vn)
    | Uint_sext -> Printf.sprintf "sext(%s)" (string_of_varnode vn)
    | Uint_2comp -> Printf.sprintf "-%s" (string_of_varnode vn)
    | Uint_negate -> Printf.sprintf "~%s" (string_of_varnode vn)
    | Ubool_negate -> Printf.sprintf "!%s" (string_of_varnode vn)
    | Ufloat_neg -> Printf.sprintf "f-%s" (string_of_varnode vn)
    | Ufloat_abs -> Printf.sprintf "abs(%s)" (string_of_varnode vn)
    | Ufloat_sqrt -> Printf.sprintf "sqrt(%s)" (string_of_varnode vn)
    | Ufloat_ceil -> Printf.sprintf "ceil(%s)" (string_of_varnode vn)
    | Ufloat_floor -> Printf.sprintf "floor(%s)" (string_of_varnode vn)
    | Ufloat_round -> Printf.sprintf "round(%s)" (string_of_varnode vn)
    | Ufloat_nan -> Printf.sprintf "nan(%s)" (string_of_varnode vn)
    | Uint2float -> Printf.sprintf "int2float(%s)" (string_of_varnode vn)
    | Ufloat2float -> Printf.sprintf "float2float(%s)" (string_of_varnode vn)
    | Utrunc -> Printf.sprintf "trunc(%s)" (string_of_varnode vn)
   )
   | Abop (op, vn1, vn2) -> (match op with
    | Bpiece -> Printf.sprintf "%s::%s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bsubpiece -> Printf.sprintf "%s:%s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_equal -> Printf.sprintf "%s == %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_notequal -> Printf.sprintf "%s != %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_less -> Printf.sprintf "%s < %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_sless -> Printf.sprintf "%s s< %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_lessequal -> Printf.sprintf "%s <= %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_slessequal -> Printf.sprintf "%s s<= %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_add -> Printf.sprintf "%s + %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_sub -> Printf.sprintf "%s - %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_carry -> Printf.sprintf "carry(%s, %s)" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_scarry -> Printf.sprintf "scarry(%s, %s)" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_sborrow -> Printf.sprintf "sborrow(%s, %s)" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_xor -> Printf.sprintf "%s ^ %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_and -> Printf.sprintf "%s & %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_or -> Printf.sprintf "%s | %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_left -> Printf.sprintf "%s << %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_right -> Printf.sprintf "%s >> %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_sright -> Printf.sprintf "%s s>> %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_mult -> Printf.sprintf "%s * %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_div -> Printf.sprintf "%s / %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_rem -> Printf.sprintf "%s %% %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_sdiv -> Printf.sprintf "%s s/ %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bint_srem -> Printf.sprintf "%s s%% %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bbool_xor -> Printf.sprintf "%s ^^ %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bbool_and -> Printf.sprintf "%s && %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bbool_or -> Printf.sprintf "%s || %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_equal -> Printf.sprintf "%s f== %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_notequal -> Printf.sprintf "%s f!= %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_less -> Printf.sprintf "%s f< %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_lessequal -> Printf.sprintf "%s f<= %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_add -> Printf.sprintf "%s f+ %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_sub -> Printf.sprintf "%s f- %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_mult -> Printf.sprintf "%s f* %s" (string_of_varnode vn1) (string_of_varnode vn2)
    | Bfloat_div -> Printf.sprintf "%s f/ %s" (string_of_varnode vn1) (string_of_varnode vn2)
   )

let string_of_pcode (p: inst) =
  match p with
  | Iload (i0, i1, o) -> Printf.sprintf "%s = *[%s]%s;" (string_of_varnode o) (string_of_varnode i0) (string_of_varnode i1)
  | Istore (i0, i1, i2) -> Printf.sprintf "*[%s]%s = %s;" (string_of_varnode i0) (string_of_varnode i1) (string_of_varnode i2)
  | Ijump (a, i) -> (match a with
    | Jbranch -> Printf.sprintf "goto %s;" (string_of_varnode i)
    | Jcall -> Printf.sprintf "call %s;" (string_of_varnode i)
  )
  | Ijump_ind (a, i) -> (match a with
    | JIbranch -> Printf.sprintf "goto *%s;" (string_of_varnode i)
    | JIcall -> Printf.sprintf "call *%s;" (string_of_varnode i)
    | JIret -> Printf.sprintf "return %s;" (string_of_varnode i)
  )
  | Icbranch (i0, i1) -> Printf.sprintf "if %s goto %s;" (string_of_varnode i1) (string_of_varnode i0)
  | Iassignment (i, o) -> Printf.sprintf "%s = %s;" (string_of_varnode o) (string_of_assignable i)
  | Iunimplemented -> "unimplemented"
;;


let recvbuf = Bytes.create 1024;;
let sendbuf = Bytes.create 9;;

(* little endian *)
let get_int (fd: Unix.file_descr): int32 =
  let recv_num = Unix.recv fd recvbuf 0 4 [] in
  assert (recv_num = 4);
  let s = Bytes.sub_string recvbuf 0 4 in
  let i = ref 0l in
  for j = 0 to 3 do
    i := Int32.logor !i (Int32.shift_left (Int32.of_int (Char.code s.[j])) (j * 8))
  done;
  !i;;
  
let get_long (fd: Unix.file_descr): int64 =
  let _ = Unix.recv fd recvbuf 0 8 [] in
  let s = Bytes.sub_string recvbuf 0 8 in
  let i = ref 0L in
  for j = 0 to 7 do
    i := Int64.logor !i (Int64.shift_left (Int64.of_int (Char.code s.[j])) (j * 8))
  done;
  !i;;

let string_of_spaceinfo (s: spaceinfo) =
  Printf.sprintf "{unique=%ld; register=%ld; const=%ld}" s.unique s.register s.const;;

let get_stateinfo (fd: Unix.file_descr) : spaceinfo =
  let unique = get_int fd in
  let register = get_int fd in
  let const = get_int fd in
  {unique; register; const};;


let string_of_varnode_raw (v: varnode_raw) =
  Printf.sprintf "{space=%ld; offset=%Ld; size=%ld}" v.space v.offset v.size;;

let get_varnode_raw (fd: Unix.file_descr) : varnode_raw =
  let space = get_int fd in
  let offset = get_long fd in
  let size = get_int fd in
  {space; offset; size};;

let tmpReg = {
   varNode_node = Unique 0L;
   varNode_width = 0l
   };;

let varnode_raw_to_varnode (si: spaceinfo) (v: varnode_raw) : varNode =
  if v.space = si.unique then
    { varNode_node = Unique v.offset;  varNode_width = v.size }
  else if v.space = si.register then
    { varNode_node = Register v.offset; varNode_width = v.size }
  else if v.space = si.const then
    { varNode_node = Const v.offset; varNode_width = v.size }
  else
    failwith (Printf.sprintf "Unknown space %ld" v.space);;


let string_of_pcode_raw (p: pcode_raw) =
  Printf.sprintf "{opcode=%ld; inputs=%s; output=%s}" p.opcode (String.concat "; " (Array.to_list (Array.map string_of_varnode_raw p.inputs))) (match p.output with None -> "None" | Some v -> string_of_varnode_raw v);;


let get_pcode_raw (fd: Unix.file_descr) : pcode_raw =
  let opcode = get_int fd in
  let num_inputs = get_int fd in
  let inputs = Array.init (Int32.to_int num_inputs) (fun _ -> get_varnode_raw fd) in
  let exists_output = get_int fd in
  let output = if exists_output = 0l then None else Some (get_varnode_raw fd) in
  {opcode; inputs; output};;

let get_pcode_list (fd: Unix.file_descr) : pcode_raw list =
  print_endline "Getting pcode list";
  let num_pcodes = get_int fd in
  print_endline (Printf.sprintf "Number of pcodes: %ld" num_pcodes);
  let rec loop acc = function
    | 0 -> acc
    | n -> loop ((get_pcode_raw fd)::acc) (n - 1) in
  List.rev (loop [] (Int32.to_int num_pcodes));;


let pcode_raw_to_pcode (si: spaceinfo) (p: pcode_raw) : inst =
  print_endline (Printf.sprintf "Converting %s" (string_of_pcode_raw p));
  let inputs i = varnode_raw_to_varnode si p.inputs.(i) in
  let output _ = varnode_raw_to_varnode si (Option.get p.output) in
  let mkJump a = Ijump (a, inputs 0) in
  let mkJIump a = Ijump_ind (a, inputs 0) in  
  let mkUop op = Iassignment (Auop (op, inputs 0), output ()) in
  let mkBop op = Iassignment (Abop (op, inputs 0, inputs 1), output ()) in
  match p.opcode with
  | 0l -> Iunimplemented
  | 1l -> Iassignment (Avar (inputs 0), output ())
  | 2l -> Iload (inputs 0, inputs 1, output ())
  | 3l -> Istore (inputs 0, inputs 1, inputs 2)
  | 4l -> mkJump Jbranch
  | 5l -> Icbranch (inputs 0, inputs 1)
  | 6l -> mkJIump JIbranch
  | 7l -> mkJump Jcall
  | 8l -> mkJIump JIcall
  | 9l -> Iunimplemented
  | 10l -> mkJIump JIret
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
  | _ -> Iunimplemented;;

