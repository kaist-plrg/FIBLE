module Inner =
  InstF.Make7 (ILoadStore) (IAssignment) (INop) (ICbranch) (IJump) (IJumpInd)
    (IUnimplemented)

include Inner
include InstFullF.Make (Inner)
