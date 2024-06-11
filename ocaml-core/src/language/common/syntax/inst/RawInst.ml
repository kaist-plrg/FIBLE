module Assignable = AssignableF.Make (NumericVarNode)
module ILoadStore = ILoadStore.Make (NumericVarNode)
module IAssignment = IAssignment.Make (NumericVarNode) (Assignable)
module INop = INop
module ICbranch = ICbranch.Make (NumericVarNode)
module IJump = IJump
module IJumpInd = IJumpInd.Make (NumericVarNode)
module IUnimplemented = IUnimplemented

module Inner =
  InstF.Make7 (ILoadStore) (IAssignment) (INop) (ICbranch) (IJump) (IJumpInd)
    (IUnimplemented)

include Inner
include InstFullF.Make (Inner)
