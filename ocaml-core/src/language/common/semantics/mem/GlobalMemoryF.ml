module type S = sig
  module Value : ValueF.S

  type t

  val from_rom : DMem.t -> t
  val load_mem : t -> Byte8.t -> Int32.t -> Value.t
  val load_string : t -> Byte8.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Byte8.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Byte8.t -> Value.t -> t
  val store_bytes : t -> Byte8.t -> String.t -> t
end

module Make (Value : ValueF.S) = struct
  module Value = Value

  type t = { left : DMemCombinedFailableMemory.t; right : Value.t Byte8Map.t }

  let from_rom (rom : DMem.t) =
    { left = DMemCombinedFailableMemory.from_rom rom; right = Byte8Map.empty }

  let load_mem (s : t) (addr : Byte8.t) (width : Int32.t) : Value.t =
    match
      Byte8Map.find_opt addr s.right
      |> Fun.flip Option.bind (fun v ->
             if Value.width v = width then Some v else None)
    with
    | Some v -> v
    | None -> (
        match
          let* res = DMemCombinedFailableMemory.load_mem s.left addr width in
          Ok res
        with
        | Ok v -> Value.of_num v
        | Error _ -> Value.undefined width)

  let load_string (s : t) (addr : Byte8.t) : (String.t, String.t) Result.t =
    DMemCombinedFailableMemory.load_string s.left addr

  let load_bytes (s : t) (addr : Byte8.t) (size : Int32.t) :
      (String.t, String.t) Result.t =
    DMemCombinedFailableMemory.load_bytes s.left addr size

  let store_mem (s : t) (addr : Byte8.t) (v : Value.t) : t =
    match Value.try_num v with
    | Error _ ->
        {
          left = DMemCombinedFailableMemory.undef_mem s.left addr 8l;
          right = Byte8Map.add addr v s.right;
        }
    | Ok v ->
        {
          left = DMemCombinedFailableMemory.store_mem s.left addr v;
          right = Byte8Map.remove addr s.right;
        }

  let store_bytes (s : t) (addr : Byte8.t) (v : String.t) : t =
    {
      left = DMemCombinedFailableMemory.store_bytes s.left addr v;
      right = Byte8Map.remove addr s.right;
    }
end
