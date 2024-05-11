type t = {
  opcode : OpTpl.op_t;
  inputs : VarNode.t array;
  output : VarNode.t option;
}

let make opcode inputs output = { opcode; inputs; output }

let pp fmt { opcode; inputs; output } =
  Format.fprintf fmt "@[<hov 2>%a=%a(%a)@]"
    (Format.pp_print_option VarNode.pp)
    output OpTpl.pp_op opcode
    (Format.pp_print_list VarNode.pp)
    (Array.to_list inputs)
