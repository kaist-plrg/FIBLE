open Common

module Inner = struct
  type t = {
    nameo : String.t Option.t;
    entry : Loc.t;
    boundaries : LocSet.t;
    sp_boundary : Int64.t * Int64.t;
    sp_diff : Int64.t;
    inputs : RegId.t List.t;
    outputs : RegId.t List.t;
    blocks : Block.t List.t;
  }
  [@@deriving sexp, show, fields]
end

include Inner
include Common.FuncHelperF.Make (Jmp) (Block) (Inner)
