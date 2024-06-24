type t = AddrSpace.t List.t

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let child = XmlExt.children xml in
  let* spaces = List.map AddrSpace.decode child |> Result.join_list in
  AddrSpace.const_space :: spaces |> Result.ok

let get_constant_space (v : t) : (AddrSpace.t, String.t) Result.t =
  AddrSpace.const_space |> Result.ok

let get_space_by_name (v : t) (name : String.t) :
    (AddrSpace.t, String.t) Result.t =
  List.find_opt (fun x -> AddrSpace.get_name x = name) v
  |> Option.to_result ~none:"AddrSpace not found"

let to_spaceinfo (v : t) : (SpaceInfo.t, String.t) Result.t =
  let* const_space = get_constant_space v in
  let* unique_space = get_space_by_name v "unique" in
  let* register_space = get_space_by_name v "register" in
  let* ram_space = get_space_by_name v "ram" in
  {
    SpaceInfo.const = AddrSpace.get_index const_space |> Int32.of_int;
    SpaceInfo.unique = AddrSpace.get_index unique_space |> Int32.of_int;
    SpaceInfo.register = AddrSpace.get_index register_space |> Int32.of_int;
    SpaceInfo.ram = AddrSpace.get_index ram_space |> Int32.of_int;
  }
  |> Result.ok
