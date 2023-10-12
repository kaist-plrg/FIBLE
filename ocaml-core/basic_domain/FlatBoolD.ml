include FlatD.Make (struct
  include Bool

  let pp fmt = function
    | true -> Format.pp_print_string fmt "true"
    | false -> Format.pp_print_string fmt "false"
end)
