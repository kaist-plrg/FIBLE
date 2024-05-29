open Common
module Inner = InstF.Make4 (ILoadStore) (ISLoadStore) (IAssignment) (INop)
include Inner
include InstFullF.Make (Inner)
