module Make (G : PointerF.S) (L : PointerF.S) = struct
  type t = (G.t, L.t) Either.t
end
