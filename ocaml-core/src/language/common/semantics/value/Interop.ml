type int_tag_prim = T64 | T32 | T16 | T8 [@@deriving show]
type int_tag = Prim of int_tag_prim | Id of String.t [@@deriving show]
type float_tag = TFloat | TDouble [@@deriving show]
type arith_tag = TInt of int_tag | TFloat of float_tag [@@deriving show]
type length_type = ZeroEnd | Dependent of String.t [@@deriving show]

type prim_tag = TArith of arith_tag | TPtr of tag | TAny

and fixed_tag =
  | TPrim of prim_tag
  | TBuffer of (arith_tag * Int64.t)
  | TIBuffer of (fixed_tag * Int64.t)
  | TStruct of fixed_tag list
  | Abs of (String.t * int_tag_prim) * fixed_tag

and dynamic_tag =
  | TArr of (arith_tag * length_type)
  | TIArr of (fixed_tag * length_type)

and tag = Fixed of fixed_tag | Dynamic of dynamic_tag [@@deriving show]

let t8 = TArith (TInt (Prim T8))
let t16 = TArith (TInt (Prim T16))
let t32 = TArith (TInt (Prim T32))
let t64 = TArith (TInt (Prim T64))
let tany = TAny
let id x = TArith (TInt (Id x))
let immutable_charbuffer_of x = TPtr (Dynamic (TIArr (TPrim t8, Dependent x)))

let mutable_charbuffer_of x =
  TPtr (Dynamic (TArr (TInt (Prim T8), Dependent x)))

let immutable_charbuffer_fixed l = TPtr (Fixed (TIBuffer (TPrim t8, l)))
let mutable_charbuffer_fixed l = TPtr (Fixed (TBuffer (TInt (Prim T8), l)))

let const_string_ptr =
  TPtr (Dynamic (TIArr (TPrim (TArith (TInt (Prim T8))), ZeroEnd)))

type func_sig = {
  params : (String.t * int_tag_prim) List.t * prim_tag List.t;
  result : prim_tag Option.t;
}
[@@deriving show]

type env_t = (String.t * int_tag_prim) List.t
(*
   \exists X. { p: Id X, : Id X

   type expression -> {
     type value,
     getter: Store -> value,
     setter: Store -> value -> Store
     }
*)

let rec contains_use_id (t : fixed_tag) (x : String.t) : Bool.t =
  match t with
  | TPrim (TArith (TInt (Id y))) -> x = y
  | TPrim (TPtr (Fixed t)) -> contains_use_id t x
  | TStruct ts -> List.exists (fun t -> contains_use_id t x) ts
  | _ -> false

let rec subst_id (t : tag) (x : String.t) (pr : int_tag_prim) (n : Int64.t) :
    tag =
  match t with
  | Fixed f -> Fixed (subst_id_fixed f x pr n)
  | Dynamic (TArr (t, Dependent y)) when x = y -> Fixed (TBuffer (t, n))
  | Dynamic (TIArr (t, Dependent y)) when x = y -> Fixed (TIBuffer (t, n))
  | _ -> t

and subst_id_fixed (f : fixed_tag) (x : String.t) (pr : int_tag_prim)
    (n : Int64.t) : fixed_tag =
  match f with
  | TPrim a -> TPrim (subst_id_prim a x pr n)
  | Abs ((y, pt), t) -> Abs ((y, pt), subst_id_fixed t x pr n)
  | TBuffer (t, l) -> TBuffer (t, l)
  | TIBuffer (t, l) -> TIBuffer (t, l)
  | TStruct ts -> TStruct (List.map (fun t -> subst_id_fixed t x pr n) ts)

and subst_id_prim (p : prim_tag) (x : String.t) (pr : int_tag_prim)
    (n : Int64.t) : prim_tag =
  match p with
  | TArith (TInt (Id y)) when x = y -> TArith (TInt (Prim pr))
  | TArith a -> TArith a
  | TPtr t -> TPtr (subst_id t x pr n)
  | TAny -> TAny

let rec typecheck (t : tag) (env : env_t) : Bool.t =
  match t with
  | Fixed f -> typecheck_fixed f env
  | Dynamic d -> typecheck_dynamic d env

and typecheck_fixed (f : fixed_tag) (env : env_t) : Bool.t =
  match f with
  | Abs ((x, pt), t) ->
      if List.exists (fun (y, _) -> x = y) env then false
      else typecheck_fixed t ((x, pt) :: env)
  | TPrim (TArith a) -> typecheck_arith a env
  | TPrim (TPtr t) -> typecheck t env
  | TPrim TAny -> true
  | TBuffer (t, l) -> typecheck_arith t env
  | TIBuffer (t, l) -> typecheck_fixed t env
  | TStruct ts -> List.for_all (fun t -> typecheck_fixed t env) ts

and typecheck_dynamic (d : dynamic_tag) (env : env_t) : Bool.t =
  match d with
  | TArr (t, ZeroEnd) -> typecheck_arith t env
  | TArr (t, Dependent x) -> List.exists (fun (y, _) -> x = y) env
  | TIArr (t, ZeroEnd) -> typecheck_fixed t env
  | TIArr (t, Dependent x) -> List.exists (fun (y, _) -> x = y) env

and typecheck_arith (a : arith_tag) (env : env_t) : Bool.t =
  match a with
  | TInt (Id x) -> List.exists (fun (y, _) -> x = y) env
  | TInt _ -> true
  | TFloat _ -> true

type int_value =
  | V64 of Int64.t
  | V32 of Int32.t
  | V16 of Int32.t
  | V8 of Char.t
[@@deriving show]

type float_value =
  | VFloat of Float32.t
  | VDouble of Float64.t
  | VLDouble of Float80.t
[@@deriving show]

type arith_value = VInt of int_value | VFloat of float_value [@@deriving show]

type t =
  | VArith of arith_value
  | VBuffer of Bytes.t
  | VIBuffer of t Array.t
  | VStruct of t List.t
  | VOpaque
[@@deriving show]

let v8 (x : Char.t) : t = VArith (VInt (V8 x))
let v16 (x : Int32.t) : t = VArith (VInt (V16 x))
let v32 (x : Int32.t) : t = VArith (VInt (V32 x))
let v64 (x : Int64.t) : t = VArith (VInt (V64 x))

let vstring (x : String.t) : t =
  VIBuffer
    (Array.of_list (List.map (fun x -> v8 x) (String.to_seq x |> List.of_seq)))

type 'value_t side_t = ('value_t * Bytes.t) List.t

let arith_is_zero (v : arith_value) : Bool.t =
  match v with
  | VInt (V64 i) -> Int64.equal i 0L
  | VInt (V32 i) -> Int32.equal i 0l
  | VInt (V16 i) -> Int32.equal i 0l
  | VInt (V8 i) -> Char.equal i '\x00'
  | VFloat (VFloat f) -> Float32.equal f Float32.zero
  | VFloat (VDouble f) -> Float64.equal f Float64.zero
  | VFloat (VLDouble f) -> Float80.equal f Float80.zero

let arith_from_int64 (t : int_tag_prim) (n : Int64.t) : arith_value =
  match t with
  | T64 -> VInt (V64 n)
  | T32 -> VInt (V32 (Int64.to_int32 n))
  | T16 -> VInt (V16 (Int64.to_int32 n))
  | T8 -> VInt (V8 (Char.chr (Int64.to_int n)))

let arith_int_size (p : int_tag_prim) : Int32.t =
  match p with T64 -> 8l | T32 -> 4l | T16 -> 2l | T8 -> 1l

let arith_float_size (p : float_tag) : Int32.t =
  match p with TFloat -> 4l | TDouble -> 8l

let prim_size (pr : prim_tag) (env : env_t) : (Int32.t, String.t) Result.t =
  match pr with
  | TArith (TInt (Id x)) ->
      let* p = List.assoc_opt x env |> Option.to_result ~none:"not found" in
      arith_int_size p |> Result.ok
  | TArith (TInt (Prim p)) -> arith_int_size p |> Result.ok
  | TArith (TFloat f) -> arith_float_size f |> Result.ok
  | TPtr _ -> 8l |> Result.ok
  | TAny -> 8l |> Result.ok

let rec fixed_size (p : fixed_tag) (env : env_t) : (Int32.t, String.t) Result.t
    =
  match p with
  | Abs ((x, pr), t) -> fixed_size t ((x, pr) :: env)
  | TPrim pr -> prim_size pr env
  | TBuffer (t, l) ->
      let* size = prim_size (TArith t) env in
      Int32.mul size (Int64.to_int32 l) |> Result.ok
  | TIBuffer (t, l) ->
      let* size = fixed_size t env in
      Int32.mul size (Int64.to_int32 l) |> Result.ok
  | TStruct ts ->
      Result.fold_left_M
        (fun acc t ->
          let* size = fixed_size t env in
          Int32.add acc size |> Result.ok)
        0l ts

let vbuffer_to_string (vs : arith_value Array.t) : String.t =
  Array.fold_left
    (fun acc v ->
      match v with
      | VInt (V64 i) -> acc ^ Int64.to_string_le i
      | VInt (V32 i) ->
          acc ^ (String.sub (Int64.to_string_le (Int64.of_int32 i))) 0 4
      | VInt (V16 i) ->
          acc ^ String.sub (Int64.to_string_le (Int64.of_int32 i)) 0 2
      | VInt (V8 i) ->
          acc ^ String.sub (Int64.to_string_le (Int64.of_int (Char.code i))) 0 1
      | VFloat (VFloat f) ->
          let i = Float32.to_bytes f |> Bytes.to_string in
          acc ^ i
      | VFloat (VDouble f) ->
          let i = Float64.to_bytes f |> Bytes.to_string in
          acc ^ i
      | VFloat (VLDouble f) ->
          let i = Float80.to_bytes f |> Bytes.to_string in
          acc ^ i)
    "" vs

let vibuffer_to_string (vs : t Array.t) : String.t =
  Array.fold_left
    (fun acc v ->
      match v with
      | VArith (VInt (V64 i)) -> acc ^ Int64.to_string_le i
      | VArith (VInt (V32 i)) ->
          acc ^ String.sub (Int64.to_string_le (Int64.of_int32 i)) 0 4
      | VArith (VInt (V16 i)) ->
          acc ^ String.sub (Int64.to_string_le (Int64.of_int32 i)) 0 2
      | VArith (VInt (V8 i)) ->
          acc ^ String.sub (Int64.to_string_le (Int64.of_int (Char.code i))) 0 1
      | VArith (VFloat (VFloat f)) ->
          let i = Float32.to_bytes f |> Bytes.to_string in
          acc ^ i
      | VArith (VFloat (VDouble f)) ->
          let i = Float64.to_bytes f |> Bytes.to_string in
          acc ^ i
      | VArith (VFloat (VLDouble f)) ->
          let i = Float80.to_bytes f |> Bytes.to_string in
          acc ^ i
      | _ -> acc)
    "" vs

let create_bytes (vs : arith_value Array.t) : Bytes.t =
  Bytes.of_string (vbuffer_to_string vs)

type ('store_t, 'value_t) interface_t = {
  load_mem : 'store_t -> 'value_t -> Int32.t -> ('value_t, String.t) Result.t;
  load_bytes : 'store_t -> 'value_t -> Int32.t -> (String.t, String.t) Result.t;
  store_bytes :
    'store_t -> 'value_t -> String.t -> ('store_t, String.t) Result.t;
  try_num : 'value_t -> (NumericValue.t, String.t) Result.t;
  add_num : 'value_t -> Int64.t -> ('value_t, String.t) Result.t;
}

type ('store_t, 'value_t) acccessor_t = {
  prim_acc :
    'store_t ->
    prim_tag ->
    'value_t ->
    (('value_t * Bytes.t) List.t * t, String.t) Result.t;
  fsig_acc :
    'store_t ->
    func_sig ->
    'value_t List.t ->
    (('value_t * Bytes.t) List.t * t List.t, String.t) Result.t;
}

let get_accessor (i : ('store_t, 'value_t) interface_t) :
    ('store_t, 'value_t) acccessor_t =
  let rec aux_ptr (s : 'store_t) (a : tag) (ptr : 'value_t) (env : env_t) :
      (('value_t * Bytes.t) List.t * t, String.t) Result.t =
    match a with
    | Fixed (TPrim pr) ->
        let* size = prim_size pr env in
        let* v = i.load_mem s ptr size in
        aux_prim s pr v []
    | Fixed (TBuffer (tag, len)) ->
        let* tsize = prim_size (TArith tag) env in
        let* ptrs =
          List.init (Int64.to_int len) (fun v ->
              i.add_num ptr (Int64.mul (Int64.of_int v) (Int64.of_int32 tsize)))
          |> Result.join_list
        in
        let* sides, rets =
          Result.fold_left_M
            (fun (sides, vs) ptr ->
              let* v = i.load_mem s ptr tsize in
              let* rsides, rv = aux_arith s tag v [] in
              (List.append rsides sides, rv :: vs) |> Result.ok)
            ([], []) ptrs
        in

        let rets = create_bytes (Array.of_list (List.rev rets)) in
        ((ptr, rets) :: sides, VBuffer rets) |> Result.ok
    | Fixed (TIBuffer (tag, len)) ->
        let* tsize = fixed_size tag env in
        let* ptrs =
          List.init (Int64.to_int len) (fun v ->
              i.add_num ptr (Int64.mul (Int64.of_int v) (Int64.of_int32 tsize)))
          |> Result.join_list
        in
        let* sides, rets =
          Result.fold_left_M
            (fun (sides, vs) ptr ->
              let* rsides, rv = aux_ptr s (Fixed tag) ptr env in
              (List.append rsides sides, rv :: vs) |> Result.ok)
            ([], []) ptrs
        in

        (sides, VIBuffer (Array.of_list (List.rev rets))) |> Result.ok
    | Fixed (TStruct tags) ->
        let* sides, rets, _ =
          Result.fold_left_M
            (fun (sides, vs, offset) t ->
              let* nptr = i.add_num ptr (Int64.of_int32 offset) in
              let* size = fixed_size t env in
              let* rsides, rv = aux_ptr s (Fixed t) nptr env in
              let noffset = Int32.add offset size in
              (List.append rsides sides, rv :: vs, noffset) |> Result.ok)
            ([], [], 0l) tags
        in
        (sides, VStruct (List.rev rets)) |> Result.ok
    | Fixed (Abs ((x, pr), t)) ->
        let size = arith_int_size pr in
        let* n = aux_extract_id_ptr s x size (Fixed t) ptr ((x, pr) :: env) in
        aux_ptr s (Fixed (subst_id_fixed t x pr n)) ptr ((x, pr) :: env)
    | Dynamic (TArr (tag, ZeroEnd)) ->
        let* tsize = prim_size (TArith tag) env in
        let rec aux_arr (k : Int.t) (sides : ('value_t * Bytes.t) List.t)
            (vs : arith_value List.t) :
            (('value_t * Bytes.t) List.t * t, String.t) Result.t =
          let* nptr =
            i.add_num ptr (Int64.mul (Int64.of_int k) (Int64.of_int32 tsize))
          in
          let* v = i.load_mem s nptr tsize in
          let* rsides, rv = aux_arith s tag v [] in
          if arith_is_zero rv then
            let retv = create_bytes (Array.of_list (List.rev vs)) in
            ((ptr, retv) :: sides, VBuffer retv) |> Result.ok
          else aux_arr (k + 1) (List.append rsides sides) (rv :: vs)
        in
        aux_arr 0 [] []
    | Dynamic (TIArr (tag, ZeroEnd)) ->
        let* tsize = fixed_size tag env in
        let rec aux_arr (k : Int.t) (sides : ('value_t * Bytes.t) List.t)
            (vs : t List.t) :
            (('value_t * Bytes.t) List.t * t, String.t) Result.t =
          let* nptr =
            i.add_num ptr (Int64.mul (Int64.of_int k) (Int64.of_int32 tsize))
          in
          let* chars =
            List.init (Int32.to_int tsize) (fun v ->
                i.add_num nptr (Int64.of_int v))
            |> Result.join_list
          in
          let* chars =
            chars
            |> List.map (fun v ->
                   let* _, c = aux_arith s (TInt (Prim T8)) v env in
                   c |> Result.ok)
            |> Result.join_list
          in

          let is_zero = List.for_all (fun v -> arith_is_zero v) chars in
          if is_zero then
            let retv = Array.of_list (List.rev vs) in
            (sides, VIBuffer retv) |> Result.ok
          else
            let* rsides, rv = aux_ptr s (Fixed tag) ptr env in
            aux_arr (k + 1) (List.append rsides sides) (rv :: vs)
        in
        aux_arr 0 [] []
    | _ -> Error "unreachable 4"
  and aux_prim (s : 'store_t) (a : prim_tag) (v : 'value_t) (env : env_t) :
      (('value_t * Bytes.t) List.t * t, String.t) Result.t =
    match a with
    | TArith t ->
        let* sides, rv = aux_arith s t v env in
        (sides, VArith rv) |> Result.ok
    | TPtr t -> aux_ptr s t v env
    | TAny -> ([], VOpaque) |> Result.ok
  and aux_arith (s : 'store_t) (a : arith_tag) (v : 'value_t) (env : env_t) :
      (('value_t * Bytes.t) List.t * arith_value, String.t) Result.t =
    match a with
    | TInt (Id x) -> Error "unreachable Id"
    | TInt (Prim t) ->
        let* n = i.try_num v in
        let* n64 = NumericValue.value_64 n in
        ([], arith_from_int64 t n64) |> Result.ok
    | TFloat TFloat ->
        let* n = i.try_num v in
        let* n64 = NumericValue.try_string n in
        let f = Float32.of_bytes (n64 |> Bytes.of_string) in
        ([], VFloat (VFloat f)) |> Result.ok
    | TFloat TDouble ->
        let* n = i.try_num v in
        let* n64 = NumericValue.try_string n in
        let f = Float64.of_bytes (n64 |> Bytes.of_string) in
        ([], VFloat (VDouble f)) |> Result.ok
  and aux_extract_id_prim (s : 'store_t) (x : String.t) (size : Int32.t)
      (t : prim_tag) (v : 'value_t) (env : env_t) : (Int64.t, String.t) Result.t
      =
    match t with
    | TArith (TInt (Id y)) when x = y ->
        let* n = i.try_num v in
        NumericValue.value_64 n
    | TPtr t -> aux_extract_id_ptr s x size t v env
    | _ -> Error "unreachable 5"
  and aux_extract_id_ptr (s : 'store_t) (x : String.t) (size : Int32.t)
      (t : tag) (ptr : 'value_t) (env : env_t) : (Int64.t, String.t) Result.t =
    match t with
    | Fixed (Abs ((y, pr), t)) ->
        aux_extract_id_ptr s x size (Fixed t) ptr ((y, pr) :: env)
    | Fixed (TPrim p) ->
        let* v = i.load_mem s ptr size in
        aux_extract_id_prim s x size p v env
    | Fixed (TStruct ts) -> (
        let* idx_opt, size =
          Result.fold_left_M
            (fun (acc, i) t ->
              match acc with
              | Some _ -> (acc, i) |> Result.ok
              | None ->
                  if contains_use_id t x then (Some t, i) |> Result.ok
                  else
                    let* size = fixed_size t env in
                    (None, Int32.add i size) |> Result.ok)
            (None, 0l) ts
        in
        match idx_opt with
        | Some t ->
            let* nptr = i.add_num ptr (Int64.of_int32 size) in
            aux_extract_id_ptr s x size (Fixed t) nptr env
        | None -> Error "unreachable 1")
    | Fixed (TBuffer _) | Fixed (TIBuffer _) | Dynamic _ ->
        Error "unreachable 2"
  in

  let aux_extract_id_list (s : 'store_t) ((x, prim) : String.t * int_tag_prim)
      (t : prim_tag List.t) (ptrs : 'value_t List.t) (env : env_t) :
      (Int64.t, String.t) Result.t =
    if List.length t <> List.length ptrs then Error "different length"
    else
      let offopt =
        List.find_opt
          (fun (t, ptr) -> contains_use_id (TPrim t) x)
          (List.combine t ptrs)
      in
      match offopt with
      | Some (t, ptr) ->
          [%log debug "found id: %s, %a" x pp_prim_tag t];
          aux_extract_id_prim s x (arith_int_size prim) t ptr env
      | None -> Error "can't find id"
  in

  let aux (s : 'store_t) (a : prim_tag) (v : 'value_t) :
      (('value_t * Bytes.t) List.t * t, String.t) Result.t =
    aux_prim s a v []
  in

  let aux_fsig (s : 'store_t) (fsig : func_sig) (v : 'value_t List.t) :
      (('value_t * Bytes.t) List.t * t List.t, String.t) Result.t =
    let tags = snd fsig.params in
    if List.length tags <> List.length v then Error "different length"
    else
      let* lens =
        List.map (fun x -> aux_extract_id_list s x tags v []) (fst fsig.params)
        |> Result.join_list
      in
      let id_with_lens = List.combine (fst fsig.params) lens in
      [%log
        debug "ids: %a"
          (List.pp (fun ppf ((x, _), l) -> Format.fprintf ppf "(%s, %Ld)" x l))
          id_with_lens];
      let* ntags =
        Result.fold_left_M
          (fun tags ((x, pr), l) ->
            List.map (fun t -> subst_id_prim t x pr l) tags |> Result.ok)
          tags id_with_lens
      in
      let* new_tags = List.combine ntags v |> Result.ok in
      let* vs =
        List.map (fun (t, v) -> aux s t v) new_tags |> Result.join_list
      in
      let sides, rets = List.split vs in
      (List.flatten sides, rets) |> Result.ok
  in
  { prim_acc = aux; fsig_acc = aux_fsig }

let get_value (of_num : NumericValue.t -> 'value) (v : arith_value) :
    ('value, String.t) Result.t =
  of_num (NumericValue.of_int64 0L 8l) |> Result.ok

let apply_side_effects (i : ('store_t, 'value_t) interface_t) (s : 'store)
    (vs : ('value * arith_value Array.t) List.t) : ('store, String.t) Result.t =
  Ok s

let extract_64_int_value (v : int_value) : Int64.t =
  match v with
  | V64 i -> i
  | V32 i -> Int64.of_int32 i
  | V16 i -> Int64.of_int32 i
  | V8 i -> Int64.of_int (Char.code i)

let extract_64 (v : t) : Int64.t =
  match v with
  | VArith (VInt i) -> extract_64_int_value i
  | _ -> [%log error "extract_64"]
