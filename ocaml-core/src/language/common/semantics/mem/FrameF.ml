open StdlibExt

module type S = sig
  module Value : ValueF.S

  type t

  val empty : Byte8.t -> Byte8.t -> t
  val load_mem : t -> Byte8.t -> Int32.t -> Value.t
  val load_string : t -> Byte8.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Byte8.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Byte8.t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> Byte8.t -> String.t -> (t, String.t) Result.t
end

module Make (Value : ValueF.S) = struct
  module Value = Value

  let ( let* ) = Result.bind

  type t = {
    left : FailableMemory.t;
    right : Value.t Byte8Map.t;
    min_addr : Byte8.t;
    max_addr : Byte8.t;
  }

  let empty (min_addr : Byte8.t) (max_addr : Byte8.t) =
    { left = FailableMemory.empty; right = Byte8Map.empty; min_addr; max_addr }

  let load_mem (s : t) (addr : Byte8.t) (width : Int32.t) : Value.t =
    if
      s.min_addr <= addr
      && Byte8.add addr (Byte8.pred (Byte8.of_int32 width)) <= s.max_addr
    then
      match
        Byte8Map.find_opt addr s.right
        |> Fun.flip Option.bind (fun v ->
               if Value.width v = width then Some v else None)
      with
      | Some v -> v
      | None -> (
          match
            let* res = FailableMemory.load_mem s.left addr width in
            Ok res
          with
          | Ok v -> Value.of_num v
          | Error _ -> Value.undefined width)
    else Value.undefined width

  let load_string (s : t) (addr : Byte8.t) : (String.t, String.t) Result.t =
    FailableMemory.load_string s.left addr

  let load_bytes (s : t) (addr : Byte8.t) (size : Int32.t) :
      (String.t, String.t) Result.t =
    FailableMemory.load_bytes s.left addr size

  let store_mem (s : t) (addr : Byte8.t) (v : Value.t) : (t, String.t) Result.t
      =
    if
      s.min_addr <= addr
      && Int64.add addr (Int64.pred (Int64.of_int32 (Value.width v)))
         <= s.max_addr
    then
      match Value.try_num v with
      | Error _ ->
          {
            s with
            left = FailableMemory.undef_mem s.left addr 8l;
            right = Byte8Map.add addr v s.right;
          }
          |> Result.ok
      | Ok v ->
          {
            s with
            left = FailableMemory.store_mem s.left addr v;
            right = Byte8Map.remove addr s.right;
          }
          |> Result.ok
    else
      Format.asprintf "%Ld out of bound [%Ld, %Ld]" addr s.min_addr s.max_addr
      |> Result.error

  let store_bytes (s : t) (addr : Byte8.t) (v : String.t) :
      (t, String.t) Result.t =
    if
      s.min_addr <= addr
      && Int64.add addr (Int64.of_int (String.length v)) <= s.max_addr
    then { s with left = FailableMemory.store_bytes s.left addr v } |> Result.ok
    else
      Format.asprintf "%Ld out of bound [%Ld, %Ld]" addr s.min_addr s.max_addr
      |> Result.error
end
