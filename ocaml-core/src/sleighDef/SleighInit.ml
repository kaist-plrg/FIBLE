open StdlibExt
open Notation

type t = {
  maxdelayslotbytes : Int32.t;
  unique_allocatemask : Int32.t;
  numSections : Int32.t;
  version : Int32.t;
  bigendian : Bool.t;
  align : Int32.t;
  uniqbase : Int64.t;
  sourcefiles : SourceFileIndexer.t;
  spaces : Spaces.t;
}

let get_constant_space (s : t) = Spaces.get_constant_space s.spaces

let get_space_by_name (s : t) (name : string) =
  Spaces.get_space_by_name s.spaces name
