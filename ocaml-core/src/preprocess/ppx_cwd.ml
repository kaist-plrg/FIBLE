open Ppxlib
module Builder = Ast_builder.Default

let expand ~ctxt _ : expression =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let pwd = Sys.getcwd () in
  (* remove path after _build *)
  let pwd = Str.global_replace (Str.regexp "_build/.*") "" pwd in
  let pwd = Builder.estring ~loc pwd in
  pwd

let my_extension =
  Extension.V3.declare "pwd" Extension.Context.expression
    Ppxlib.Ast_pattern.(__)
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "pwd"
