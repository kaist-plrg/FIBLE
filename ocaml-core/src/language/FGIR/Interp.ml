open Common
open Syn
open Sem

let step_call_internal (s : State.t) (p : Prog.t)
    ({ target = { target = calln; attr }; attr = (); fallthrough } : SCall.t) :
    (Store.t * Stack.elem_t, StopEvent.t) Result.t =
  let* currf = State.get_current_function s p |> StopEvent.of_str_res in
  let* f = State.get_func_from p calln |> StopEvent.of_str_res in

  (* TODO: think ind copydepth
     let* _ =
       if snd f.sp_boundary <= copydepth then Ok ()
       else Error "jcall_ind: copydepth not match"
     in
  *)
  (s.sto, (s.cursor, fallthrough)) |> Result.ok

let step_ret (s : State.t) (p : Prog.t) ({ attr } : SRet.t) (calln, retn') :
    (Store.t, StopEvent.t) Result.t =
  let* attr = Value.try_loc attr |> StopEvent.of_str_res in
  if Loc.compare attr retn' <> 0 then
    StopEvent.FailStop "Retaddr not matched" |> Result.error
  else Ok s.sto

let action_JC = State.mk_action_JC step_call_internal
let action_JR = State.mk_action_JR step_ret

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Cursor.t) :
    (StoreAction.t, String.t) Result.t =
  Inst.fold (Store.step_ILS s) (Store.step_IA s) (Store.step_IN s)
    (Store.step_SP s) ins

let step_jmp (p : Prog.t) (jmp : Jmp.t) (s : State.t) :
    (Action.t, String.t) Result.t =
  match jmp with
  | JI ji -> State.step_JI p s ji
  | JC jc -> (
      let* sc = SCall.eval s.sto jc in
      match Byte8Map.find_opt (Loc.get_addr sc.target.target) p.externs with
      | None -> Action.call sc |> Result.ok
      | Some name -> State.step_call_external p s.sto name sc.fallthrough)
  | JR jr ->
      let* sr = SRet.eval s.sto jr in
      Action.ret sr |> Result.ok
  | JT _ -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (Action.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = { ins = Third _; _ } :: []; jmp } ->
      step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = i :: []; jmp = { jmp = JI (JIntraF.Jfallthrough l) } } ->
      let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
      Action.of_store a (Some l) |> Result.ok
  | { remaining = i :: res; jmp } ->
      if List.is_empty res then
        StopEvent.FailStop "Not possible inst" |> Result.error
      else
        let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
        Action.of_store a None |> Result.ok

let action_store (p : Prog.t) (sto : Store.t) (a : StoreAction.t) (s : State.t)
    : (Store.t, StopEvent.t) Result.t =
  match a with
  | Special "syscall" -> (
      let* rax =
        Store.get_reg sto { id = RegId.Register 0l; offset = 0l; width = 8l }
        |> Value.value_64 |> StopEvent.of_str_res
      in
      [%log finfo "syscall" "SYSCALL NUM: %Ld" rax];
      match rax with
      | 3L ->
          Store.add_reg sto
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 0L 8l)
          |> Result.ok
      | 9L ->
          let* rdi =
            Store.get_reg sto
              { id = RegId.Register 56l; offset = 0l; width = 8l }
            |> Value.value_64 |> StopEvent.of_str_res
          in
          let* r8 =
            Store.get_reg sto
              { id = RegId.Register 128l; offset = 0l; width = 8l }
            |> Value.value_64 |> StopEvent.of_str_res
          in
          if r8 = 0xffffffffffffffffL then
            if rdi = 0L then
              let hval =
                (String.hash (Format.asprintf "%a%a" Store.pp sto State.pp s)
                |> Int64.of_int |> Int64.shift_left)
                  24
              in
              Store.add_reg sto
                { id = RegId.Register 0l; offset = 0l; width = 8l }
                (Value.of_int64 hval 8l)
              |> Result.ok
            else
              Store.add_reg sto
                { id = RegId.Register 0l; offset = 0l; width = 8l }
                (Value.of_int64 rdi 8l)
              |> Result.ok
          else Error "Not supported mmap" |> StopEvent.of_str_res
      | 11L ->
          Store.add_reg sto
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 0L 8l)
          |> Result.ok
      | 12L ->
          let* rdi =
            Store.get_reg sto
              { id = RegId.Register 56l; offset = 0l; width = 8l }
            |> Value.value_64 |> StopEvent.of_str_res
          in
          Store.add_reg sto
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 rdi 8l)
          |> Result.ok
      | 16L ->
          Store.add_reg sto
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 (0 |> Int64.of_int) 8l)
          |> Result.ok
      | 20L ->
          let* rdi =
            Store.get_reg sto
              { id = RegId.Register 56l; offset = 0l; width = 8l }
            |> Value.value_64 |> StopEvent.of_str_res
          in
          let rsi =
            Store.get_reg sto
              { id = RegId.Register 48l; offset = 0l; width = 8l }
          in
          let* rdx =
            Store.get_reg sto
              { id = RegId.Register 16l; offset = 0l; width = 8l }
            |> Value.value_64 |> StopEvent.of_str_res
          in
          [%log
            finfo "syscall" "%a: SYSCALL ARG: %Ld %a %Ld" Loc.pp
              (Cont.get_loc (State.get_cont s))
              rdi Value.pp rsi rdx];
          let* writestr =
            Result.fold_left_M
              (fun strr n ->
                let* ptr_1 =
                  NumericBop.eval Bint_add rsi
                    (Value.of_int64 (n * 16 |> Int64.of_int) 8l)
                    8l
                in
                let* ptr_2 =
                  NumericBop.eval Bint_add rsi
                    (Value.of_int64 ((n * 16) + 8 |> Int64.of_int) 8l)
                    8l
                in
                let* v1 = Store.load_mem sto ptr_1 8l in
                let* v2 = Store.load_mem sto ptr_2 8l in
                let* len = Value.value_32 v2 in
                let* strra = Store.load_bytes sto v1 len in
                Ok (strr ^ strra))
              ""
              (List.init (Int64.to_int rdx) Fun.id)
            |> StopEvent.of_str_res
          in
          Out_channel.output_string Stdlib.stdout writestr;
          Store.add_reg sto
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 (String.length writestr |> Int64.of_int) 8l)
          |> Result.ok
      | _ ->
          [%log finfo "syscall" "%a" Stack.pp s.stack];
          Error (Format.sprintf "unimplemented syscall %Ld" rax)
          |> StopEvent.of_str_res)
  | Special "LOCK" | Special "UNLOCK" -> sto |> Result.ok
  | Special x ->
      Error (Format.sprintf "unimplemented special %s" x)
      |> StopEvent.of_str_res
  | Assign (p, v) -> Store.action_assign sto p v |> StopEvent.of_str_res
  | Load (r, p, v) -> Store.action_load sto r p v |> StopEvent.of_str_res
  | Store (p, v) -> Store.action_store sto p v |> StopEvent.of_str_res
  | Nop -> Store.action_nop sto |> StopEvent.of_str_res

let action (p : Prog.t) (s : State.t) (a : Action.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = action_store p s.sto a s in
      match (lo, s.cont) with
      | None, { remaining = _ :: res; jmp } ->
          Ok { s with sto; cont = { remaining = res; jmp } }
      | Some l, _ ->
          let* cont =
            Cont.of_loc p (State.get_func_loc s) l |> StopEvent.of_str_res
          in
          Ok { s with sto; cont }
      | _ -> StopEvent.FailStop "Not possible inst" |> Result.error)
  | Jmp l -> State.action_jmp p s l |> StopEvent.of_str_res
  | ExternCall (name, values, args, ft) -> (
      match World.Environment.request_call_opt name args with
      | Some (sides, retv) -> State.action_extern p s values sides retv ft
      | None -> StopEvent.NormalStop |> Result.error)
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let action_with_computed_extern (p : Prog.t) (s : State.t) (a : Action.t)
    (sides : (Int.t * Interop.t) List.t) (retv : Interop.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = action_store p s.sto a s in
      match (lo, s.cont) with
      | None, { remaining = _ :: res; jmp } ->
          Ok { s with sto; cont = { remaining = res; jmp } }
      | Some l, _ ->
          let* cont =
            Cont.of_loc p (State.get_func_loc s) l |> StopEvent.of_str_res
          in
          Ok { s with sto; cont }
      | _ -> StopEvent.FailStop "Not possible inst" |> Result.error)
  | Jmp l -> State.action_jmp p s l |> StopEvent.of_str_res
  | ExternCall (name, values, args, ft) ->
      State.action_extern p s values sides retv ft
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let* a =
    step p s |> Fun.flip StopEvent.add_loc (Cont.get_loc (State.get_cont s))
  in
  let* s' =
    action p s a |> Fun.flip StopEvent.add_loc (Cont.get_loc (State.get_cont s))
  in
  interp p s'
