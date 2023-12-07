open StdlibExt
open Basic
open Basic_collection

module Make (Value : sig
  type t

  module NonNumericValue : sig
    type t

    val width : t -> Int32.t
    val undefined : Int32.t -> t
  end

  val to_either : t -> (NumericValue.t, NonNumericValue.t) Either.t
  val of_either : (NumericValue.t, NonNumericValue.t) Either.t -> t
end) =
struct
  let ( let* ) = Result.bind

  type t = {
    left : FailableMemory.t;
    right : Value.NonNumericValue.t AddrMap.t;
  }

  let empty = { left = FailableMemory.empty; right = AddrMap.empty }

  let load_mem (s : t) (addr : Addr.t) (width : Int32.t) : Value.t =
    match
      AddrMap.find_opt addr s.right
      |> Fun.flip Option.bind (fun v ->
             if Value.NonNumericValue.width v = width then Some v else None)
    with
    | Some v -> Value.of_either (Right v)
    | None -> (
        match
          let* res = FailableMemory.load_mem s.left addr width in
          Ok (Value.of_either (Left res))
        with
        | Ok v -> v
        | Error _ ->
            Value.of_either (Right (Value.NonNumericValue.undefined width)))

  let load_string (s : t) (addr : Addr.t) : (String.t, String.t) Result.t =
    FailableMemory.load_string s.left addr

  let load_bytes (s : t) (addr : Addr.t) (size : Int32.t) :
      (String.t, String.t) Result.t =
    FailableMemory.load_bytes s.left addr size

  let store_mem (s : t) (addr : Addr.t) (v : Value.t) : t =
    match Value.to_either v with
    | Right v ->
        {
          left = FailableMemory.undef_mem s.left addr 8l;
          right = AddrMap.add addr v s.right;
        }
    | Left v ->
        {
          left = FailableMemory.store_mem s.left addr v;
          right = AddrMap.remove addr s.right;
        }

  let store_bytes (s : t) (addr : Addr.t) (v : String.t) : t =
    { s with left = FailableMemory.store_bytes s.left addr v }
end
