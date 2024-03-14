open StdlibExt
open Basic_domain

(*
   module Inner = struct
     type t = SP of Int64.t | Const of Int64.t | RVal

     let pp fmt x =
       match x with
       | SP x -> Format.fprintf fmt "SP %Ld" x
       | Const x -> Format.fprintf fmt "Const %Ld" x
       | RVal -> Format.fprintf fmt "RVal"
   end

   include FlatD.Make (Inner)
*)
type __ = { have_sp : FlatBoolD.t; offset : FlatInt64D.t }

include
  TupleD.MakeLatticeWithTop_Record (FlatBoolD) (FlatInt64D)
    (struct
      type t = __

      let get_fst x = x.have_sp
      let get_snd x = x.offset
      let make x y = { have_sp = x; offset = y }
    end)

let bottom = { have_sp = FlatBoolD.Bot; offset = FlatInt64D.Bot }

let pp fmt x =
  Format.fprintf fmt "{ have_sp = %a; offset = %a }" FlatBoolD.pp x.have_sp
    FlatInt64D.pp x.offset

let of_const (x : Int64.t) =
  { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat x }

let bop_const bop x y width =
  match (x, y) with
  | ( { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat x },
      { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat y } ) ->
      { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat (bop x y) }
  | _ -> top

let uop_const uop x =
  match x with
  | { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat x } ->
      { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat (uop x) }
  | _ -> top

let add x y width =
  match (x, y) with
  | ( { have_sp = FlatBoolD.Flat true; offset = FlatInt64D.Flat x },
      { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat y } ) ->
      {
        have_sp = FlatBoolD.Flat true;
        offset = FlatInt64D.Flat (Int64.add x y);
      }
  | ( { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat x },
      { have_sp = FlatBoolD.Flat true; offset = FlatInt64D.Flat y } ) ->
      {
        have_sp = FlatBoolD.Flat true;
        offset = FlatInt64D.Flat (Int64.add x y);
      }
  | ( { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat x },
      { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat y } ) ->
      {
        have_sp = FlatBoolD.Flat false;
        offset = FlatInt64D.Flat (Int64.add x y);
      }
  | _ -> top

let sub x y width =
  match (x, y) with
  | ( { have_sp = FlatBoolD.Flat true; offset = FlatInt64D.Flat x },
      { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat y } ) ->
      {
        have_sp = FlatBoolD.Flat true;
        offset = FlatInt64D.Flat (Int64.sub x y);
      }
  | ( { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat x },
      { have_sp = FlatBoolD.Flat false; offset = FlatInt64D.Flat y } ) ->
      {
        have_sp = FlatBoolD.Flat false;
        offset = FlatInt64D.Flat (Int64.sub x y);
      }
  | _ -> top

let mul x y width = bop_const Int64.mul x y width

let sext x inwidth outwidth =
  uop_const (fun x -> Int64Ext.sext x inwidth outwidth) x
