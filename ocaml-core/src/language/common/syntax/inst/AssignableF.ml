type 'varnode_t poly_t =
  | Avar of 'varnode_t
  | Auop of (Uop.t * 'varnode_t)
  | Abop of (Bop.t * 'varnode_t * 'varnode_t)
[@@deriving sexp]

module Make (VarNode : sig
  type t

  val pp : Format.formatter -> t -> unit
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
end) =
struct
  type t = VarNode.t poly_t [@@deriving sexp]

  let pp (fmt : Format.formatter) (a : t) =
    match a with
    | Avar vn -> VarNode.pp fmt vn
    | Auop (op, vn) -> (
        match op with
        | Upopcount -> Format.fprintf fmt "popcount(%a)" VarNode.pp vn
        | Ulzcount -> Format.fprintf fmt "lzcount(%a)" VarNode.pp vn
        | Uint_zext -> Format.fprintf fmt "zext(%a)" VarNode.pp vn
        | Uint_sext -> Format.fprintf fmt "sext(%a)" VarNode.pp vn
        | Uint_2comp -> Format.fprintf fmt "-%a" VarNode.pp vn
        | Uint_negate -> Format.fprintf fmt "~%a" VarNode.pp vn
        | Ubool_negate -> Format.fprintf fmt "!%a" VarNode.pp vn
        | Ufloat_neg -> Format.fprintf fmt "f-%a" VarNode.pp vn
        | Ufloat_abs -> Format.fprintf fmt "abs(%a)" VarNode.pp vn
        | Ufloat_sqrt -> Format.fprintf fmt "sqrt(%a)" VarNode.pp vn
        | Ufloat_ceil -> Format.fprintf fmt "ceil(%a)" VarNode.pp vn
        | Ufloat_floor -> Format.fprintf fmt "floor(%a)" VarNode.pp vn
        | Ufloat_round -> Format.fprintf fmt "round(%a)" VarNode.pp vn
        | Ufloat_nan -> Format.fprintf fmt "nan(%a)" VarNode.pp vn
        | Uint2float -> Format.fprintf fmt "int2float(%a)" VarNode.pp vn
        | Ufloat2float -> Format.fprintf fmt "float2float(%a)" VarNode.pp vn
        | Utrunc -> Format.fprintf fmt "trunc(%a)" VarNode.pp vn)
    | Abop (op, vn1, vn2) -> (
        match op with
        | Bpiece -> Format.fprintf fmt "%a::%a" VarNode.pp vn1 VarNode.pp vn2
        | Bsubpiece -> Format.fprintf fmt "%a:%a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_equal ->
            Format.fprintf fmt "%a == %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_notequal ->
            Format.fprintf fmt "%a != %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_less ->
            Format.fprintf fmt "%a < %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_sless ->
            Format.fprintf fmt "%a s< %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_lessequal ->
            Format.fprintf fmt "%a <= %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_slessequal ->
            Format.fprintf fmt "%a s<= %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_add -> Format.fprintf fmt "%a + %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_sub -> Format.fprintf fmt "%a - %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_carry ->
            Format.fprintf fmt "carry(%a, %a)" VarNode.pp vn1 VarNode.pp vn2
        | Bint_scarry ->
            Format.fprintf fmt "scarry(%a, %a)" VarNode.pp vn1 VarNode.pp vn2
        | Bint_sborrow ->
            Format.fprintf fmt "sborrow(%a, %a)" VarNode.pp vn1 VarNode.pp vn2
        | Bint_xor -> Format.fprintf fmt "%a ^ %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_and -> Format.fprintf fmt "%a & %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_or -> Format.fprintf fmt "%a | %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_left ->
            Format.fprintf fmt "%a << %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_right ->
            Format.fprintf fmt "%a >> %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_sright ->
            Format.fprintf fmt "%a s>> %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_mult ->
            Format.fprintf fmt "%a * %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_div -> Format.fprintf fmt "%a / %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_rem ->
            Format.fprintf fmt "%a %% %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_sdiv ->
            Format.fprintf fmt "%a s/ %a" VarNode.pp vn1 VarNode.pp vn2
        | Bint_srem ->
            Format.fprintf fmt "%a s%% %a" VarNode.pp vn1 VarNode.pp vn2
        | Bbool_xor ->
            Format.fprintf fmt "%a ^^ %a" VarNode.pp vn1 VarNode.pp vn2
        | Bbool_and ->
            Format.fprintf fmt "%a && %a" VarNode.pp vn1 VarNode.pp vn2
        | Bbool_or ->
            Format.fprintf fmt "%a || %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_equal ->
            Format.fprintf fmt "%a f== %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_notequal ->
            Format.fprintf fmt "%a f!= %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_less ->
            Format.fprintf fmt "%a f< %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_lessequal ->
            Format.fprintf fmt "%a f<= %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_add ->
            Format.fprintf fmt "%a f+ %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_sub ->
            Format.fprintf fmt "%a f- %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_mult ->
            Format.fprintf fmt "%a f* %a" VarNode.pp vn1 VarNode.pp vn2
        | Bfloat_div ->
            Format.fprintf fmt "%a f/ %a" VarNode.pp vn1 VarNode.pp vn2)
end
