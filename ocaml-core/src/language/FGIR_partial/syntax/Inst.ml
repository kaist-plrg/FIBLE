open Common
module Assignable = AssignableF.Make (VarNode)
module ILoadStore = ILoadStore.Make (VarNode)
module IAssignment = IAssignment.Make (VarNode) (Assignable)
module INop = INop
module Inner = InstF.Make3 (ILoadStore) (IAssignment) (INop)
include Inner
include InstFullF.Make (Inner)
