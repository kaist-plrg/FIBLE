module Value = Common_language.NumericValue
module Store = Common_language.LowStore

module State = struct
  open Basic

  module Inner = struct
    type t = { sto : Store.t; pc : Loc.t }

    let set_store (s : t) (sto : Store.t) : t = { s with sto }
    let set_pc (s : t) (pc : Loc.t) : t = { s with pc }
    let get_store (s : t) : Store.t = s.sto
    let get_pc (s : t) : Loc.t = s.pc
    let pp fmt (s : t) : unit = Format.fprintf fmt "pc: %a\n" Loc.pp s.pc
  end

  include Inner
  include Common_language.StateStoreF.Make (Value) (Store) (Inner)
end
