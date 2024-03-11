open StdlibExt
open Notation
open Basic
open Basic_collection

module Make (Prog : sig
  type t
end) (Value : sig
  type t

  val pp : Format.formatter -> t -> unit
  val try_loc : t -> (Loc.t, String.t) Result.t
  val try_isZero : t -> (bool, String.t) Result.t
end) (Store : sig
  type t

  val add_reg : t -> RegId.t_full -> Value.t -> t
  val get_reg : t -> RegId.t_full -> Value.t
  val load_mem : t -> Value.t -> Int32.t -> (Value.t, String.t) Result.t
  val load_string : t -> Value.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Value.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Value.t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> Value.t -> String.t -> (t, String.t) Result.t
  val eval_vn : t -> VarNode.t -> (Value.t, String.t) Result.t
end) (Cont : sig
  type t

  val pp : Format.formatter -> t -> unit
  val of_loc : Prog.t -> Loc.t -> Loc.t -> (t, String.t) Result.t
end) (Stack : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (Cursor : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_func_loc : t -> Loc.t
end) (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = {
    sto : Store.t;
    cont : Cont.t;
    stack : Stack.t;
    cursor : Cursor.t;
    attr : Attr.t;
  }

  let get_store (s : t) : Store.t = s.sto
  let set_store (s : t) (sto : Store.t) : t = { s with sto }
  let get_cont (s : t) : Cont.t = s.cont
  let set_cont (s : t) (cont : Cont.t) : t = { s with cont }
  let get_func_loc (s : t) : Loc.t = Cursor.get_func_loc s.cursor

  let pp fmt (s : t) : unit =
    Format.fprintf fmt "cursor: %a\n cont: %a\nstack: %a\nattr: %a\n" Cursor.pp
      s.cursor Cont.pp s.cont Stack.pp s.stack Attr.pp s.attr

  let step_JI (s : t) (p : Prog.t) (j : JIntra.t) : (t, String.t) Result.t =
    match j with
    | Jjump l ->
        let* ncont = Cont.of_loc p (get_func_loc s) l in
        set_cont s ncont |> Result.ok
    | Jfallthrough l ->
        let* ncont = Cont.of_loc p (get_func_loc s) l in
        set_cont s ncont |> Result.ok
    | Jjump_ind { target; candidates; _ } ->
        let* tv = Store.eval_vn (get_store s) target in
        let* loc = Value.try_loc tv in
        if LocSet.mem loc candidates then
          let* ncont = Cont.of_loc p (get_func_loc s) loc in
          set_cont s ncont |> Result.ok
        else "jump_ind: Not a valid jump" |> Result.error
    | Jcbranch { condition; target_true; target_false } ->
        let* v = Store.eval_vn (get_store s) condition in
        [%log debug "Jcbranch %a" Value.pp v];
        let* iz = Value.try_isZero v in
        if iz then
          let* ncont = Cont.of_loc p (get_func_loc s) target_false in
          set_cont s ncont |> Result.ok
        else
          let* ncont = Cont.of_loc p (get_func_loc s) target_true in
          set_cont s ncont |> Result.ok
    | Junimplemented -> "unimplemented jump" |> Result.error
end
