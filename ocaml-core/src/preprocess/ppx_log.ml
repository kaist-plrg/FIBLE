open Ppxlib
module Builder = Ast_builder.Default

let expand ~ctxt (la : label) (alist : (arg_label * expression) list) :
    expression =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Builder.eapply ~loc
    (Builder.pexp_ident ~loc
       { txt = Ldot (Ldot (Lident "StdlibExt", "Logger"), la); loc })
    (Builder.estring ~loc loc.loc_start.pos_fname
    :: Builder.eint ~loc loc.loc_start.pos_lnum
    :: List.map snd alist)

let my_extension =
  Extension.V3.declare "log" Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_apply (pexp_ident (lident __)) __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "log"
