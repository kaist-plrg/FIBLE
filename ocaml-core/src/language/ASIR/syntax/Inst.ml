open Common
module Assignable = AssignableF.Make (VarNode)
module ISLoadStore = ISLoadStore.Make (VarNode)
module ILoadStore = ILoadStore.Make (VarNode)
module IAssignment = IAssignment.Make (VarNode) (Assignable)
module INop = INop
module Inner = InstF.Make4 (ILoadStore) (ISLoadStore) (IAssignment) (INop)
include Inner
include InstFullF.Make (Inner)
