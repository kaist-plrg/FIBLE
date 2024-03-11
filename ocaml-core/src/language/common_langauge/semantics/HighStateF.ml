open StdlibExt
open Notation
open Basic
open Basic_collection

module Make (Func : sig
  type t
end) (Prog : sig
  type t

  val get_func_opt : t -> Loc.t -> Func.t option
  val get_externs : t -> String.t AddrMap.t
end) (CallTarget : sig
  module Attr : sig
    type t
  end

  type t
  type resolved_t

  val to_either : t -> (Loc.t * Attr.t, VarNode.t) Either.t
  val mk_direct : Loc.t -> Attr.t -> resolved_t
  val mk_indirect : Loc.t -> resolved_t
  val get_target_resolved : resolved_t -> Loc.t
end) (JCall : sig
  module Attr : sig
    type t
  end

  type t
  type resolved_t

  val get_target : t -> CallTarget.t
  val get_target_resolved : resolved_t -> CallTarget.resolved_t
  val get_attr_resolved : resolved_t -> Attr.t
  val get_fallthrough_resolved : resolved_t -> Loc.t
  val to_resolved : t -> CallTarget.resolved_t -> resolved_t
end) (JRet : sig
  type t
end) (TimeStamp : sig
  type t

  val pp : Format.formatter -> t -> unit
  val succ : t -> t
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

  val build_args :
    t -> Interop.func_sig -> ((Value.t * Interop.t) List.t, String.t) Result.t

  val build_sides :
    t -> Value.t List.t -> (Int.t * Interop.t) List.t -> (t, String.t) Result.t

  val build_ret : t -> Interop.t -> (t, String.t) Result.t
  val add_sp_extern : t -> Prog.t -> (Value.t, String.t) Result.t
  val sp_extern : Prog.t -> RegId.t_full
end) (Cont : sig
  type t

  val pp : Format.formatter -> t -> unit
  val of_loc : Prog.t -> Loc.t -> Loc.t -> (t, String.t) Result.t
  val of_func_entry_loc : Prog.t -> Loc.t -> (t, String.t) Result.t
end) (Cursor : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_func_loc : t -> Loc.t
  val get_timestamp : t -> TimeStamp.t
  val make : Loc.t -> TimeStamp.t -> t
end) (Stack : sig
  type elem_t
  type t = elem_t List.t

  val get_cursor : elem_t -> Cursor.t
  val get_fallthrough : elem_t -> Loc.t
  val pp : Format.formatter -> t -> unit
end) (Environment : sig
  type hidden_fn

  val signature_map : (Interop.func_sig * hidden_fn) StringMap.t

  val request_call_opt :
    String.t ->
    Interop.t list ->
    ((Int.t * Interop.t) list * Interop.t) Option.t
end) =
struct
  type t = {
    sto : Store.t;
    cont : Cont.t;
    stack : Stack.t;
    cursor : Cursor.t;
    timestamp : TimeStamp.t;
  }

  let get_store (s : t) : Store.t = s.sto
  let set_store (s : t) (sto : Store.t) : t = { s with sto }
  let get_cont (s : t) : Cont.t = s.cont
  let set_cont (s : t) (cont : Cont.t) : t = { s with cont }
  let get_func_loc (s : t) : Loc.t = Cursor.get_func_loc s.cursor

  let pp fmt (s : t) : unit =
    Format.fprintf fmt "cursor: %a\n cont: %a\nstack: %a\ntimestamp: %a\n"
      Cursor.pp s.cursor Cont.pp s.cont Stack.pp s.stack TimeStamp.pp
      s.timestamp

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

  let step_call_external (s : Store.t) (p : Prog.t) (name : String.t)
      (jcr : JCall.resolved_t) : (Store.t, StopEvent.t) Result.t =
    [%log debug "Calling %s" name];
    let* fsig, _ =
      StringMap.find_opt name Environment.signature_map
      |> Option.to_result
           ~none:
             (StopEvent.FailStop
                (Format.asprintf "No external function %s" name))
    in
    let* bargs = Store.build_args s fsig |> StopEvent.of_str_res in
    let values, args = bargs |> List.split in
    [%log
      debug "Call values: %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Value.pp)
        values];
    [%log
      debug "Call args: %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Interop.pp)
        args];
    let* sides, retv =
      match Environment.request_call_opt name args with
      | Some (sides, retv) -> Ok (sides, retv)
      | None -> Error StopEvent.NormalStop
    in
    [%log
      debug "Side values: %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun fmt (i, v) ->
             Format.fprintf fmt "%d: %a" i Interop.pp v))
        sides];
    let* sp_saved = Store.add_sp_extern s p |> StopEvent.of_str_res in
    let* sto_side = Store.build_sides s values sides |> StopEvent.of_str_res in
    let* sto =
      Store.build_ret (Store.add_reg sto_side (Store.sp_extern p) sp_saved) retv
      |> StopEvent.of_str_res
    in
    Ok sto

  let mk_step_JC
      (s_internal :
        t ->
        Prog.t ->
        JCall.resolved_t ->
        (Store.t * Stack.elem_t, StopEvent.t) result) (s : t) (p : Prog.t)
      (jc : JCall.t) : (t, StopEvent.t) Result.t =
    let step_call (s : t) (p : Prog.t) (jcr : JCall.resolved_t) :
        (t, StopEvent.t) Result.t =
      let target_loc =
        CallTarget.get_target_resolved (JCall.get_target_resolved jcr)
      in
      match AddrMap.find_opt (Loc.to_addr target_loc) (Prog.get_externs p) with
      | None ->
          let* sto, selem = s_internal s p jcr in
          let* ncont =
            Cont.of_func_entry_loc p target_loc |> StopEvent.of_str_res
          in
          let ncursor : Cursor.t =
            Cursor.make target_loc (TimeStamp.succ s.timestamp)
          in
          Ok
            {
              timestamp = TimeStamp.succ s.timestamp;
              cont = ncont;
              stack = selem :: s.stack;
              cursor = ncursor;
              sto;
            }
      | Some name ->
          let* sto = step_call_external s.sto p name jcr in
          let* ncont =
            Cont.of_loc p
              (Cursor.get_func_loc s.cursor)
              (JCall.get_fallthrough_resolved jcr)
            |> StopEvent.of_str_res
          in
          Ok { s with cont = ncont; sto }
    in
    let* (cr : CallTarget.resolved_t) =
      match CallTarget.to_either (JCall.get_target jc) with
      | Left (target, attr) -> CallTarget.mk_direct target attr |> Result.ok
      | Right target ->
          let* calln =
            Result.bind (Store.eval_vn s.sto target) Value.try_loc
            |> StopEvent.of_str_res
          in
          CallTarget.mk_indirect calln |> Result.ok
    in
    let jcr : JCall.resolved_t = JCall.to_resolved jc cr in
    step_call s p jcr

  let mk_step_JR
      (step_ret :
        t -> Prog.t -> JRet.t -> Stack.elem_t -> (Store.t, StopEvent.t) result)
      (s : t) (p : Prog.t) (jr : JRet.t) : (t, StopEvent.t) Result.t =
    match s.stack with
    | [] -> Error StopEvent.NormalStop
    | e :: stack' ->
        let* ncont =
          Cont.of_loc p
            (Cursor.get_func_loc (Stack.get_cursor e))
            (Stack.get_fallthrough e)
          |> StopEvent.of_str_res
        in
        let cursor = Stack.get_cursor e in
        let* sto = step_ret s p jr e in
        Ok
          {
            timestamp = TimeStamp.succ s.timestamp;
            cont = ncont;
            stack = stack';
            cursor;
            sto;
          }

  let get_func_from (p : Prog.t) (target : Loc.t) : (Func.t, String.t) Result.t
      =
    Prog.get_func_opt p target
    |> Option.to_result
         ~none:(Format.asprintf "jcall: not found function %a" Loc.pp target)

  let get_current_function (s : t) (p : Prog.t) : (Func.t, String.t) Result.t =
    get_func_from p (Cursor.get_func_loc s.cursor)
end
