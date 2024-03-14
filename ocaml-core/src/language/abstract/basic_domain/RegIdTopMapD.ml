open Common

module Make (A : sig
  include DomainSpec.JoinSemiLatitceWithTop
  include PrettySpec.PrettyPrint with type t := t
end) =
struct
  include TopMapD.Make (Common.RegId) (A)

  let pp (fmt : Format.formatter) (m : t) : unit =
    let pp_pair fmt (k, v) =
      Format.fprintf fmt "%a -> %a" Common.RegId.pp k A.pp v
    in
    Format.fprintf fmt "{%a}" (Format.pp_print_list pp_pair) (bindings m)
end

module Make_Mut (A : sig
  include DomainSpec.JoinSemiLatitceWithTop
  include PrettySpec.PrettyPrint with type t := t
end) =
struct
  include TopMapD.Make_Mut (Common.RegId) (A)

  let pp (fmt : Format.formatter) (m : t) : unit =
    let pp_pair fmt (k, v) =
      Format.fprintf fmt "%a -> %a" Common.RegId.pp k A.pp v
    in
    Format.fprintf fmt "{%a}" (Format.pp_print_list pp_pair) (bindings m)
end

module MakeLatticeWithTop (A : sig
  include DomainSpec.LatticeWithTop
  include PrettySpec.PrettyPrint with type t := t
end) =
struct
  include TopMapD.MakeLatticeWithTop (Common.RegId) (A)

  let pp (fmt : Format.formatter) (m : t) : unit =
    let pp_pair fmt (k, v) =
      Format.fprintf fmt "%a -> %a" Common.RegId.pp k A.pp v
    in
    Format.fprintf fmt "{%a}" (Format.pp_print_list pp_pair) (bindings m)
end
