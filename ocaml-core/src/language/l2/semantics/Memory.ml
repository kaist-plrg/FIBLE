open Basic
open Basic_collection

let ( let* ) = Result.bind

type t = { left : Common_language.FailableMemory.t; right : SPVal.t AddrMap.t }

let empty =
  { left = Common_language.FailableMemory.empty; right = AddrMap.empty }

let load_mem (s : t) (addr : Addr.t) (width : Int32.t) : Value.t =
  if AddrMap.mem addr s.right && width = 8l then SP (AddrMap.find addr s.right)
  else
    match
      let* res = Common_language.FailableMemory.load_mem s.left addr width in
      Ok (Value.Num res)
    with
    | Ok v -> v
    | Error _ -> Undef width

let store_mem (s : t) (addr : Addr.t) (v : Value.t) : t =
  match v with
  | SP v ->
      {
        left = Common_language.FailableMemory.undef_mem s.left addr 8l;
        right = AddrMap.add addr v s.right;
      }
  | Num v ->
      {
        left = Common_language.FailableMemory.store_mem s.left addr v;
        right = AddrMap.remove addr s.right;
      }
  | Undef width ->
      {
        left = Common_language.FailableMemory.undef_mem s.left addr width;
        right = AddrMap.remove addr s.right;
      }
