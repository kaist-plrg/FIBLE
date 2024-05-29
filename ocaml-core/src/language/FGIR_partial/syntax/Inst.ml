open Common
module Inner = InstF.Make3 (ILoadStore) (IAssignment) (INop)
include Inner
include InstFullF.Make (Inner)
