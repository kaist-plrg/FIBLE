type t = { tree : Int32Set.t; parent : Int32.t Option.t; id : Int32.t }

let symbol_scope (parent : Int32.t Option.t) (id : Int32.t) =
  { tree = Int32Set.empty; parent; id }

let add_symbol (scope : t) (id : Int32.t) =
  { scope with tree = Int32Set.add id scope.tree }
