type t = { width : Int32.t; must_nonzero : Bool.t }

let pp fmt (v : t) : Unit.t =
  Format.fprintf fmt "undef_%ld" v.width;
  if v.must_nonzero then Format.fprintf fmt " (must_nonzero)"

let compare { width = width_a; must_nonzero = mz_a }
    { width = width_b; must_nonzero = mz_b } =
  let cv = Int32.compare width_a width_b in
  if cv = 0 then Bool.compare mz_a mz_b else cv

let is_must_nonzero { must_nonzero; _ } = must_nonzero

let of_width ?(must_nonzero = false) outwidth =
  { width = outwidth; must_nonzero }

let width { width; _ } = width
