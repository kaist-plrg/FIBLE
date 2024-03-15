open Common

module Inner = struct
  type t =
    | ILS of ILoadStore.t
    | ISLS of ISLoadStore.t
    | IA of IAssignment.t
    | IN of INop.t

  let pp (fmt : Format.formatter) (p : t) =
    match p with
    | ILS p -> ILoadStore.pp fmt p
    | ISLS p -> ISLoadStore.pp fmt p
    | IA p -> IAssignment.pp fmt p
    | IN p -> INop.pp fmt p

  let is_nop ins =
    match ins with
    | ILS i -> ILoadStore.is_nop i
    | ISLS i -> ISLoadStore.is_nop i
    | IA i -> IAssignment.is_nop i
    | IN i -> INop.is_nop i
end

include Inner
include InstFullF.Make (Inner)
