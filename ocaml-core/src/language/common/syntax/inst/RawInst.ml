module Assignable = AssignableF.Make (NumericVarNode)
module ILoadStore = ILoadStore.Make (NumericVarNode)
module IAssignment = IAssignment.Make (NumericVarNode) (Assignable)
module INop = INop
module ISpecial = ISpecial
module ICbranch = ICbranch.Make (NumericVarNode)
module IJump = IJump
module IJumpInd = IJumpInd.Make (NumericVarNode)
module IUnimplemented = IUnimplemented

module Inner =
  InstF.Make8 (ILoadStore) (IAssignment) (INop) (ISpecial) (ICbranch) (IJump)
    (IJumpInd)
    (IUnimplemented)

include Inner
include InstFullF.Make (Inner)
