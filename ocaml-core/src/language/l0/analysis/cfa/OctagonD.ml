open Basic
open Basic_domain
open Value_domain

module Key = struct
  type key = Kmr of MemRef.t | KZero
  type t = key * key

  let compare = compare
end

module I64D = struct
  type t = I64 of Int64.t | Top | Bot

  let compare = compare
  let top = Top

  let join a b =
    match (a, b) with
    | Bot, _ -> b
    | _, Bot -> a
    | I64 a, I64 b ->
        if a = b then I64 a
        else if Int64.max a b > 10L then Top
        else I64 (Int64.max a b)
    | _, _ -> Top

  let meet a b =
    match (a, b) with
    | Top, _ -> b
    | _, Top -> a
    | I64 a, I64 b ->
        if a = b then I64 a
        else if Int64.min a b < -10L then Bot
        else I64 (Int64.min a b)
    | _, _ -> Bot

  let le a b =
    match (a, b) with
    | Bot, _ -> true
    | _, Bot -> false
    | I64 a, I64 b -> a <= b
    | Top, I64 _ -> false
    | _, _ -> true

  let pp fmt a =
    match a with
    | I64 i -> Format.fprintf fmt "%Ld" i
    | Top -> Format.fprintf fmt "Top"
    | Bot -> Format.fprintf fmt "Bot"
end

module MemRefZeroTopMapT = TopMapD.MakeLatticeWithTop (Key) (I64D)

type t = MemRefZeroTopMapT.t

let join = MemRefZeroTopMapT.join
let le = MemRefZeroTopMapT.le
let top = MemRefZeroTopMapT.top
let meet = MemRefZeroTopMapT.meet

let refine_consts (a : t) =
  let consts =
    MemRefZeroTopMapT.filter_map
      (fun (k1, k2) v ->
        match k2 with
        | KZero -> (
            match MemRefZeroTopMapT.find_opt (KZero, k1) a with
            | Some v2 -> (
                match (v, v2) with
                | I64D.I64 i1, I64D.I64 i2 ->
                    if i1 = Int64.neg i2 then Some i1 else None
                | _, _ -> None)
            | None -> None)
        | _ -> None)
      a
  in
  MemRefZeroTopMapT.to_seq a
  |> Seq.filter_map (fun ((k1, k2), v) ->
         match
           ( k1,
             k2,
             MemRefZeroTopMapT.find_opt (k1, KZero) consts,
             MemRefZeroTopMapT.find_opt (k2, KZero) consts,
             v )
         with
         | Kmr _, _, None, Some j, I64D.I64 vi ->
             Some ((k1, Key.KZero), Int64.add vi j)
         | _, Kmr _, Some i, None, I64D.I64 vi ->
             Some ((Key.KZero, k2), Int64.sub vi i)
         | _ -> None)
  |> Seq.fold_left
       (fun acc (k, v) ->
         MemRefZeroTopMapT.update k
           (fun o ->
             match o with
             | Some o -> Some (I64D.meet o (I64D.I64 v))
             | None -> Some (I64D.I64 v))
           acc)
       a

let ole (m1 : t) (m2 : t) : bool =
  MemRefZeroTopMapT.fold
    (fun k v2 acc ->
      match MemRefZeroTopMapT.find_opt k m1 with
      | Some v1 -> acc && I64D.le v1 v2
      | None -> acc && I64D.le I64D.top v2)
    m2 true

let pp fmt (a : t) =
  MemRefZeroTopMapT.iter
    (fun (k1, k2) v ->
      match (k1, k2) with
      | Kmr mr1, Kmr mr2 ->
          Format.fprintf fmt "(%a - %a) <= %a; " MemRef.pp mr1 MemRef.pp mr2
            I64D.pp v
      | KZero, Kmr mr2 ->
          Format.fprintf fmt "-(%a) <= %a; " MemRef.pp mr2 I64D.pp v
      | Kmr mr1, KZero ->
          Format.fprintf fmt "%a <= %a; " MemRef.pp mr1 I64D.pp v
      | KZero, KZero -> Format.fprintf fmt "0 <= %a; " I64D.pp v)
    a

let clear_memref a =
  MemRefZeroTopMapT.filter
    (fun k _ ->
      match k with
      | Kmr (MemRef.ROffset _), _ -> false
      | _, Kmr (MemRef.ROffset _) -> false
      | _ -> true)
    a

let clear_mr a (mr : MemRef.t) =
  MemRefZeroTopMapT.filter
    (fun (k1, k2) _ -> compare k1 (Kmr mr) <> 0 && compare k2 (Kmr mr) <> 0)
    a

let gen_single_lt (a : t) (mr : MemRef.t) (c : int64) =
  a |> MemRefZeroTopMapT.add (Kmr mr, KZero) (I64D.I64 (Int64.pred c))

let gen_single_ge (a : t) (mr : MemRef.t) (c : int64) =
  a |> MemRefZeroTopMapT.add (KZero, Kmr mr) (I64D.I64 (Int64.neg c))

let gen_single_eq (a : t) (mr : MemRef.t) (c : int64) =
  a
  |> MemRefZeroTopMapT.add (Kmr mr, KZero) (I64D.I64 c)
  |> MemRefZeroTopMapT.add (KZero, Kmr mr) (I64D.I64 (Int64.neg c))

let find_interval_shallow (a : t) (mr : MemRef.t) =
  let minv =
    MemRefZeroTopMapT.find_opt (KZero, Kmr mr) a
    |> Option.map (fun v ->
           match v with I64D.I64 i -> IntervalD.EInt (Int64.neg i) | _ -> ETop)
    |> Option.value ~default:IntervalD.ETop
  in
  let maxv =
    MemRefZeroTopMapT.find_opt (Kmr mr, KZero) a
    |> Option.map (fun v ->
           match v with I64D.I64 i -> IntervalD.EInt i | _ -> ETop)
    |> Option.value ~default:IntervalD.ETop
  in
  (minv, maxv)

let request_interval (a : t) (mr : MemRef.t) =
  match mr with
  | MemRef.R ({ id = RegId.Unique v; _ } as reg) ->
      let mmr = MemRef.ROffset (reg, 0L) in
      let equiv_mr =
        MemRefZeroTopMapT.filter_map
          (fun (k1, k2) v ->
            match (k1, k2) with
            | Kmr mr1, Kmr mr2 ->
                if
                  v = I64D.I64 0L
                  && compare mmr mr1 = 0
                  && MemRefZeroTopMapT.find_opt (Kmr mr2, Kmr mr1) a
                     = Some (I64D.I64 0L)
                then Some mr2
                else None
            | _ -> None)
          a
        |> MemRefZeroTopMapT.to_seq
        |> Seq.map (fun (_, v) -> v)
      in
      let equiv_intervals =
        Seq.map (fun mr -> find_interval_shallow a mr) equiv_mr
      in
      let interval = find_interval_shallow a mmr in
      Seq.fold_left
        (fun acc (minv, maxv) ->
          ( IntervalD.meet_et_low minv (fst acc),
            IntervalD.meet_et_high maxv (snd acc) ))
        interval equiv_intervals
  | _ -> (IntervalD.ETop, IntervalD.ETop)

let process_load (_ : Prog.t) (a : t) (pointerv : VarNode.t) (outv : RegId.t) =
  match (pointerv, MemRef.convert_regid outv) with
  | Register ({ id = RegId.Unique _; _ } as u), outmr -> (
      match
        ( MemRefZeroTopMapT.find_opt (Kmr outmr, Kmr (MemRef.ROffset (u, 0L))) a,
          MemRefZeroTopMapT.find_opt (Kmr (MemRef.ROffset (u, 0L)), Kmr outmr) a
        )
      with
      | Some (I64D.I64 0L), Some (I64D.I64 0L) -> a
      | _ ->
          clear_mr a outmr
          |> MemRefZeroTopMapT.add
               (Kmr outmr, Kmr (MemRef.ROffset (u, 0L)))
               (I64D.I64 0L)
          |> MemRefZeroTopMapT.add
               (Kmr (MemRef.ROffset (u, 0L)), Kmr outmr)
               (I64D.I64 0L))
  | _, _ -> a

let process_assignment (a : t) (asn : Assignable.t) (outv : RegId.t) =
  match MemRef.convert_regid outv with
  | outmr -> (
      let na = clear_mr a outmr in
      match asn with
      | Avar vn -> (
          match MemRef.convert_varnode vn with
          | Some amr ->
              na
              |> MemRefZeroTopMapT.add (Kmr outmr, Kmr amr) (I64D.I64 0L)
              |> MemRefZeroTopMapT.add (Kmr amr, Kmr outmr) (I64D.I64 0L)
          | _ -> na)
      | Abop (Bint_add, op1v, op2v) -> (
          match (op1v, op2v) with
          | Register ({ id = RegId.Unique _; _ } as u), Const { value = c; _ }
            ->
              na
              |> MemRefZeroTopMapT.add
                   (Kmr outmr, Kmr (MemRef.R u))
                   (I64D.I64 c)
              |> MemRefZeroTopMapT.add
                   (Kmr (MemRef.R u), Kmr outmr)
                   (I64D.I64 (Int64.neg c))
          | Register r, Const { value = c; _ } -> (
              match
                ( MemRefZeroTopMapT.find_opt (Kmr outmr, Kmr (MemRef.R r)) a,
                  MemRefZeroTopMapT.find_opt (Kmr (MemRef.R r), Kmr outmr) a )
              with
              | Some (I64D.I64 i1), Some (I64D.I64 i2) ->
                  if c = i1 && c = Int64.neg i2 then a
                  else
                    na
                    |> MemRefZeroTopMapT.add
                         (Kmr outmr, Kmr (MemRef.R r))
                         (I64D.I64 c)
                    |> MemRefZeroTopMapT.add
                         (Kmr (MemRef.R r), Kmr outmr)
                         (I64D.I64 (Int64.neg c))
              | _ ->
                  na
                  |> MemRefZeroTopMapT.add
                       (Kmr outmr, Kmr (MemRef.R r))
                       (I64D.I64 c)
                  |> MemRefZeroTopMapT.add
                       (Kmr (MemRef.R r), Kmr outmr)
                       (I64D.I64 (Int64.neg c)))
          | _ -> na)
      | Abop (Bint_sub, op1v, op2v) -> (
          match (op1v, op2v) with
          | Register ({ id = RegId.Unique _; _ } as u), Const { value = c; _ }
            ->
              na
              |> MemRefZeroTopMapT.add
                   (Kmr outmr, Kmr (MemRef.R u))
                   (I64D.I64 (Int64.neg c))
              |> MemRefZeroTopMapT.add
                   (Kmr (MemRef.R u), Kmr outmr)
                   (I64D.I64 c)
          | _ -> na)
      | Abop (_, _, _) -> na
      | Auop (Uint_zext, vn) -> (
          match MemRef.convert_varnode vn with
          | Some amr ->
              na
              |> MemRefZeroTopMapT.add (Kmr outmr, Kmr amr) (I64D.I64 0L)
              |> MemRefZeroTopMapT.add (Kmr amr, Kmr outmr) (I64D.I64 0L)
          | _ -> na)
      | Auop (Uint_sext, vn) -> (
          match MemRef.convert_varnode vn with
          | Some amr ->
              na
              |> MemRefZeroTopMapT.add (Kmr outmr, Kmr amr) (I64D.I64 0L)
              |> MemRefZeroTopMapT.add (Kmr amr, Kmr outmr) (I64D.I64 0L)
          | _ -> na)
      | Auop (_, _) -> na)
