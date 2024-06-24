module Make (Func : sig
  type t
end) (Prog : sig
  type t

  val get_func_opt : t -> Loc.t -> Func.t option
  val get_externs : t -> String.t Byte8Map.t
end) (VarNode : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (CallTarget : sig
  module Attr : sig
    type t
  end

  type t

  val to_either : t -> (Loc.t * Attr.t, VarNode.t) Either.t
end) (JCall : sig
  type t

  val get_target : t -> CallTarget.t
end) (JTailCall : sig
  type t
end) (JRet : sig
  type t
end) (JIntra : sig
  type t = VarNode.t JIntraF.poly_t
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

  val pp : Format.formatter -> t -> unit
  val add_reg : t -> RegId.t_full -> Value.t -> t
  val get_reg : t -> RegId.t_full -> Value.t
  val load_mem : t -> Value.t -> Int32.t -> (Value.t, String.t) Result.t
  val load_string : t -> Value.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Value.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Value.t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> Value.t -> String.t -> (t, String.t) Result.t
  val eval_vn : t -> VarNode.t -> (Value.t, String.t) Result.t

  val eval_vn_list :
    t -> VarNode.t List.t -> (Value.t List.t, String.t) Result.t

  val build_args :
    t -> Interop.func_sig -> ((Value.t * Interop.t) List.t, String.t) Result.t

  val build_sides :
    t -> Value.t List.t -> (Int.t * Interop.t) List.t -> (t, String.t) Result.t

  val build_ret : t -> Interop.t -> (t, String.t) Result.t
  val add_sp_extern : t -> Prog.t -> (Value.t, String.t) Result.t
  val sp_extern : Prog.t -> RegId.t_full
end) (SCallTarget : sig
  type t

  val get_target : t -> Loc.t
end) (SCall : sig
  type t

  val get_target : t -> SCallTarget.t
  val get_fallthrough : t -> Loc.t
  val eval : Store.t -> JCall.t -> (t, String.t) Result.t
end) (STailCall : sig
  type t

  val get_target : t -> SCallTarget.t
  val eval : Store.t -> JTailCall.t -> (t, String.t) Result.t
end) (SRet : sig
  type t

  val eval : Store.t -> JRet.t -> (t, String.t) Result.t
end) (Action : sig
  type t

  module StoreAction : sig
    type t
  end

  val of_store : StoreAction.t -> Loc.t Option.t -> t
  val jmp : Loc.t -> t
  val externcall : String.t -> Value.t List.t -> Interop.t List.t -> Loc.t -> t
  val call : SCall.t -> t
  val tailcall : STailCall.t -> t
  val ret : SRet.t -> t
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
  let get_timestamp (s : t) : TimeStamp.t = s.timestamp
  let get_func_loc (s : t) : Loc.t = Cursor.get_func_loc s.cursor

  let pp fmt (s : t) : unit =
    Format.fprintf fmt
      "@[<1>sto: %a@,cursor: %a@,cont: %a@,stack: %a@,timestamp: %a@]" Store.pp
      s.sto Cursor.pp s.cursor Cont.pp s.cont Stack.pp s.stack TimeStamp.pp
      s.timestamp

  let step_JI (p : Prog.t) (s : t) (j : JIntra.t) :
      (Action.t, String.t) Result.t =
    match j with
    | Jjump l | Jfallthrough l -> Action.jmp l |> Result.ok
    | Jjump_ind { target; candidates; _ } ->
        let* tv = Store.eval_vn (get_store s) target in
        let* loc = Value.try_loc tv in
        if LocSet.mem loc candidates then Action.jmp loc |> Result.ok
        else "jump_ind: Not a valid jump" |> Result.error
    | Jcbranch { condition; target_true; target_false } ->
        let* v = Store.eval_vn (get_store s) condition in
        [%log debug "Jcbranch %a" Value.pp v];
        let* iz = Value.try_isZero v in
        if iz then Action.jmp target_false |> Result.ok
        else Action.jmp target_true |> Result.ok
    | Junimplemented -> "unimplemented jump" |> Result.error

  let action_jmp (p : Prog.t) (s : t) (l : Loc.t) : (t, String.t) Result.t =
    let* ncont = Cont.of_loc p (get_func_loc s) l in
    set_cont s ncont |> Result.ok

  let step_call_external (p : Prog.t) (s : Store.t) (name : String.t)
      (fallthrough : Loc.t) : (Action.t, String.t) Result.t =
    [%log debug "Calling %s" name];
    let* fsig, _ =
      StringMap.find_opt name Environment.signature_map
      |> Option.to_result ~none:(Format.asprintf "No external function %s" name)
    in
    let* bargs = Store.build_args s fsig in
    let values, args = bargs |> List.split in
    [%log
      debug "Call values: %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Value.pp)
        values];
    [%log
      debug "Call args: %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Interop.pp)
        args];

    Action.externcall name values args fallthrough |> Result.ok

  let action_external_sto (p : Prog.t) (s : Store.t) (values : Value.t List.t)
      (sides : (int * Interop.t) List.t) (retv : Interop.t) :
      (Store.t, StopEvent.t) Result.t =
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

  let action_extern (p : Prog.t) (s : t) (values : Value.t List.t)
      (sides : (int * Interop.t) List.t) (retv : Interop.t) (ft : Loc.t) :
      (t, StopEvent.t) Result.t =
    let* sto = action_external_sto p s.sto values sides retv in
    let* ncont =
      Cont.of_loc p (Cursor.get_func_loc s.cursor) ft |> StopEvent.of_str_res
    in
    Ok { s with cont = ncont; sto }

  let mk_action_JC
      (s_internal :
        t -> Prog.t -> SCall.t -> (Store.t * Stack.elem_t, StopEvent.t) result)
      (p : Prog.t) (s : t) (sc : SCall.t) : (t, StopEvent.t) Result.t =
    let target_loc = SCallTarget.get_target (SCall.get_target sc) in
    let* sto, selem = s_internal s p sc in
    let* ncont = Cont.of_func_entry_loc p target_loc |> StopEvent.of_str_res in
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

  let mk_action_JR
      (action_ret :
        t -> Prog.t -> SRet.t -> Stack.elem_t -> (Store.t, StopEvent.t) result)
      (p : Prog.t) (s : t) (sr : SRet.t) : (t, StopEvent.t) Result.t =
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
        let* sto = action_ret s p sr e in
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
