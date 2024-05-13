type core_t = {
  opcode : Int32.t;
  inputs : VarNode.t array;
  output : VarNode.t option;
}

type t = {
  opcode : Int32.t;
  mnemonic : string;
  inputs : VarNode.t array;
  output : VarNode.t option;
}

let make opcode inputs output = { opcode; inputs; output }

let append_mnemonic ({ opcode; inputs; output } : core_t) mnemonic =
  { opcode; inputs; output; mnemonic }

let pp fmt (p : t) =
  Format.fprintf fmt "{mnemonic: %s; opcode=%ld; inputs=%a; output=%a}"
    p.mnemonic p.opcode
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       VarNode.pp)
    (Array.to_list p.inputs)
    (fun fmt -> function
      | None -> Format.fprintf fmt "None"
      | Some v -> Format.fprintf fmt "%a" VarNode.pp v)
    p.output
