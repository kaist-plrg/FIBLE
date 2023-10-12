open StdlibExt

type t = { a_interval : IntervalD.t; a_mod8 : Mod8D.t; a_limset : LimSetD.t }

let top =
  { a_interval = IntervalD.top; a_mod8 = Mod8D.top; a_limset = LimSetD.top }

let join a b =
  {
    a_interval = IntervalD.join a.a_interval b.a_interval;
    a_mod8 = Mod8D.join a.a_mod8 b.a_mod8;
    a_limset = LimSetD.join a.a_limset b.a_limset;
  }

let meet a b =
  {
    a_interval = IntervalD.meet a.a_interval b.a_interval;
    a_mod8 = Mod8D.meet a.a_mod8 b.a_mod8;
    a_limset = LimSetD.meet a.a_limset b.a_limset;
  }

let le a b =
  IntervalD.le a.a_interval b.a_interval
  && Mod8D.le a.a_mod8 b.a_mod8
  && LimSetD.le a.a_limset b.a_limset

let pp fmt a =
  Format.fprintf fmt "(%a, %a, %a)" IntervalD.pp a.a_interval Mod8D.pp a.a_mod8
    LimSetD.pp a.a_limset

let of_const n : t =
  {
    a_interval = IntervalD.of_const n;
    a_mod8 = Mod8D.of_const n;
    a_limset = LimSetD.of_const n;
  }

let of_interval x =
  { a_interval = x; a_mod8 = Mod8D.top; a_limset = LimSetD.top }

let of_limset x =
  { a_interval = IntervalD.top; a_mod8 = Mod8D.top; a_limset = x }

let add a b width =
  {
    a_interval = IntervalD.add a.a_interval b.a_interval;
    a_mod8 = Mod8D.add a.a_mod8 b.a_mod8;
    a_limset = LimSetD.add a.a_limset b.a_limset width;
  }

let sub _ _ _ = top

let mul a b width =
  {
    a_interval = IntervalD.mul a.a_interval b.a_interval;
    a_mod8 = Mod8D.mul a.a_mod8 b.a_mod8;
    a_limset = LimSetD.mul a.a_limset b.a_limset width;
  }

let sext v in_width out_width =
  {
    a_interval = IntervalD.sext v.a_interval in_width out_width;
    a_mod8 = Mod8D.sext v.a_mod8 in_width out_width;
    a_limset = LimSetD.sext v.a_limset in_width out_width;
  }

let try_concretize (a : t) (limit : int) : Int64Set.t option =
  let s1 = IntervalD.try_concretize a.a_interval (limit * 8) in
  let s2 = LimSetD.try_concretize a.a_limset (limit * 8) in
  let ns =
    match (s1, s2) with
    | None, None -> None
    | Some s1, None -> Some s1
    | None, Some s2 -> Some s2
    | Some s1, Some s2 -> Some (Int64Set.union s1 s2)
  in
  match
    ns |> Option.map (Int64Set.filter (fun x -> Mod8D.contains a.a_mod8 x))
  with
  | None -> None
  | Some s -> if Int64Set.cardinal s > limit then None else Some s
