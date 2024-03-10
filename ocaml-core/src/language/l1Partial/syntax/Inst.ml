open Basic
open Common_language

module Inner = struct
  type t = ILS of ILoadStore.t | IA of IAssignment.t | IN of INop.t

  let pp (fmt : Format.formatter) (p : t) =
    match p with
    | ILS p -> ILoadStore.pp fmt p
    | IA p -> IAssignment.pp fmt p
    | IN p -> INop.pp fmt p

  let is_nop (p : t) =
    match p with
    | ILS p -> ILoadStore.is_nop p
    | IA p -> IAssignment.is_nop p
    | IN p -> INop.is_nop p
end

include Inner
include InstFullF.Make (Inner)
