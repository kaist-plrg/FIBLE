open Basic;;

module Make (A: sig
  include DomainSpec.JoinSemiLatitceWithTop
  include PrettySpec.PrettyPrint with type t := t
end
) = struct
  include TopMapD.Make(Basic.MemRef)(A)

  let pp (fmt: Format.formatter) (m: t): unit =
    let pp_pair fmt (k, v) = Format.fprintf fmt "%a -> %a" Basic.MemRef.pp k A.pp v in
    Format.fprintf fmt "{%a}" (Format.pp_print_list pp_pair) (bindings m)
end
