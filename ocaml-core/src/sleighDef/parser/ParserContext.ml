open StdlibExt
open Notation

type t = { instb : String.t; context : Int32.t List.t }

let getInstructionBits (v : t) (startbit : Int32.t) (size : Int32.t)
    (off : Int32.t) : (Int32.t, String.t) Result.t =
  (*
    {
      off += (startbit/8);
      if (off >= 16)
        throw BadDataError("Instruction is using more than 16 bytes");
      const uint1 *ptr = buf + off;
      startbit = startbit % 8;
      int4 bytesize = (startbit+size-1)/8 + 1;
      uintm res = 0;
      for(int4 i=0;i<bytesize;++i) {
        res <<= 8;
        res |= ptr[i];
      }
      res <<= 8*(sizeof(uintm)-bytesize)+startbit; // Move starting bit to highest position
      res >>= 8*sizeof(uintm)-size;	// Shift to bottom of intm
      return res;
    }
*)
  [%log debug "getInstructionBits startbit: %ld" startbit];
  [%log debug "getInstructionBits size: %ld" size];
  [%log debug "getInstructionBits off: %ld" off];
  let off = Int32.add off (Int32.div startbit 8l) in
  if Int32.compare off 16l >= 0 then
    "Instruction is using more than 16 bytes" |> Result.error
  else
    let startbit = Int32.rem startbit 8l in
    let bytesize =
      Int32.add (Int32.div (Int32.pred (Int32.add startbit size)) 8l) 1l
    in
    let res = 0l in
    let res =
      List.fold_left
        (fun res i ->
          Int32.shift_left res 8
          |> Int32.logor
               (Int32.of_int (Char.code v.instb.[Int32.to_int off + i])))
        res
        (List.init (Int32.to_int bytesize) Fun.id)
    in
    let res =
      Int32.shift_left res
        (Int32.to_int
           (Int32.add (Int32.mul 8l (Int32.sub 4l bytesize)) startbit))
    in
    [%log debug "getInstructionBits res: %lx" res];
    let res =
      Int32.shift_right_logical res
        (Int32.to_int (Int32.sub (Int32.mul 8l 4l) size))
    in
    [%log debug "getInstructionBits res: %lx" res];
    res |> Result.ok

let getInstructionBytes (v : t) (offset : Int32.t) (size : Int32.t)
    (off : Int32.t) : (Int32.t, String.t) Result.t =
  getInstructionBits v (Int32.mul offset 8l) (Int32.mul size 8l) off

let getContextBits (v : t) (startbit : Int32.t) (size : Int32.t) :
    (Int32.t, String.t) Result.t =
  (*
     {
       int4 intstart = startbit / (8*4);
       uintm res = context[ intstart ]; // Get intm containing highest bit
       int4 bitOffset = startbit % (8*4);
       int4 unusedBits = 8*4 - size;
       res <<= bitOffset;	// Shift startbit to highest position
       res >>= unusedBits;
       int4 remaining = size - 8*4 + bitOffset;
       if ((remaining > 0) && (++intstart < contextsize)) {
         uintm res2 = context[ intstart ];
         unusedBits = 8*4 - remaining;
         res2 >>= unusedBits;
         res |= res2;
       }
       return res;
     }
  *)
  let intstart = Int32.div startbit (Int32.mul 8l 4l) in
  let* res =
    List.nth_opt v.context (Int32.to_int intstart)
    |> Option.to_result
         ~none:(Format.sprintf "context index out of bound: %ld" intstart)
  in
  let bitOffset = Int32.rem startbit (Int32.mul 8l 4l) in
  let unusedBits = Int32.sub (Int32.mul 8l 4l) size in
  let res = Int32.shift_left res (Int32.to_int bitOffset) in
  let res = Int32.shift_right_logical res (Int32.to_int unusedBits) in
  let remaining = Int32.sub size (Int32.sub (Int32.mul 8l 4l) bitOffset) in
  if
    Int32.compare remaining 0l > 0
    && Int32.compare (Int32.add intstart 1l)
         (Int32.of_int (List.length v.context))
       < 0
  then
    let* res2 =
      List.nth_opt v.context (Int32.to_int (Int32.add intstart 1l))
      |> Option.to_result
           ~none:
             (Format.sprintf "context2 index out of bound: %d"
                (Int32.to_int (Int32.add intstart 1l)))
    in
    let unusedBits = Int32.sub (Int32.mul 8l 4l) remaining in
    let res2 = Int32.shift_right_logical res2 (Int32.to_int unusedBits) in
    let res = Int32.logor res res2 in
    res |> Result.ok
  else res |> Result.ok

let getContextBytes (v : t) (offset : Int32.t) (size : Int32.t) :
    (Int32.t, String.t) Result.t =
  getContextBits v (Int32.mul offset 8l) (Int32.mul size 8l)

(*   void setContextWord(int4 i,uintm val,uintm mask) { context[i] = (context[i]&(~mask))|(mask&val); } *)
let setContextWord (v : t) (i : Int32.t) (vb : Int32.t) (mask : Int32.t) :
    (t, String.t) Result.t =
  let* context =
    List.nth_opt v.context (Int32.to_int i)
    |> Option.to_result ~none:"context index out of bound"
  in
  [%log debug "setContextWord i: %ld" i];
  [%log debug "setContextWord vb: %lx" vb];
  [%log debug "setContextWord mask: %lx" mask];
  [%log debug "setContextWord before: %lx" context];
  let context =
    Int32.logand context (Int32.lognot mask)
    |> Int32.logor (Int32.logand mask vb)
  in
  [%log debug "setContextWord after: %lx" context];
  let context =
    List.mapi (fun i' c -> if Int32.to_int i = i' then context else c) v.context
  in
  { v with context } |> Result.ok

let of_mock (s : String.t) : t =
  {
    instb =
      String.sub
        (String.cat s
           "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")
        0 16;
    context = [ 0x89000000l; 0x0l ];
  }
