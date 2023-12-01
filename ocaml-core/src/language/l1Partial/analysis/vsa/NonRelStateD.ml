open StdlibExt
open Basic
open Basic_domain
open Value_domain
module Map = KeyMap.Make (AbsNumeric)

type t = Map.t

let le = Map.le
let top = Map.top
let join a b = Map.join a b
let meet a b = Map.meet a b
let clear_memref a = Map.clear_memref a
let clear_mr a (r : RegId.t) = Map.clear_mr a r
let pp = Map.pp
let find_opt = Map.find_opt

let eval_varnode (a : t) (d : OctagonD.t) (vn : VarNode.t) =
  match vn with
  | Const c -> AbsNumeric.of_const c.value
  | Register r -> (
      match Map.find_opt (KReg r.id) a with
      | None -> AbsNumeric.of_interval (OctagonD.request_interval d r.id)
      | Some v ->
          AbsNumeric.meet
            (AbsNumeric.of_interval (OctagonD.request_interval d r.id))
            v)
  | Ram _ -> AbsNumeric.top

let process_load (p : Prog.t) (a : t) (d : OctagonD.t) (pointerv : VarNode.t)
    (outv : RegId.t_width) =
  match pointerv with
  | Register r -> (
      match
        Option.bind
          (Map.find_loc_opt { base = r.id; offset = 0L } a)
          (fun x -> AbsNumeric.try_concretize x 20)
      with
      | Some vset ->
          Map.add (KReg outv.id)
            (AbsNumeric.of_limset
               (LimSetD.LimSet
                  (Int64Set.map
                     (fun x -> (Prog.get_rom p x (RegId.width outv)).value)
                     vset)))
            a
      | None ->
          Map.add (KReg outv.id)
            (AbsNumeric.of_interval (OctagonD.request_interval d r.id))
            a)
  | _ -> clear_mr a outv.id

let process_assignment (a : t) (d : OctagonD.t) (asn : Assignable.t)
    (outv : RegId.t_width) =
  let na = clear_mr a outv.id in
  match asn with
  | Avar vn -> Map.add (KReg outv.id) (eval_varnode a d vn) na
  | Abop (Bint_add, op1v, op2v) ->
      let vn1 = eval_varnode a d op1v in
      let vn2 = eval_varnode a d op2v in
      Map.add (KReg outv.id) (AbsNumeric.add vn1 vn2 (RegId.width outv)) na
  | Abop (Bint_sub, op1v, op2v) ->
      let vn1 = eval_varnode a d op1v in
      let vn2 = eval_varnode a d op2v in
      Map.add (KReg outv.id) (AbsNumeric.sub vn1 vn2 (RegId.width outv)) na
  | Abop (Bint_mult, op1v, op2v) ->
      let vn1 = eval_varnode a d op1v in
      let vn2 = eval_varnode a d op2v in
      Map.add (KReg outv.id) (AbsNumeric.mul vn1 vn2 (RegId.width outv)) na
  | Abop (_, _, _) -> na
  | Auop (Uint_sext, vn) ->
      let v = eval_varnode a d vn in
      Map.add (KReg outv.id)
        (AbsNumeric.sext v (VarNode.width vn) (RegId.width outv))
        na
  | Auop (Uint_zext, vn) -> Map.add (KReg outv.id) (eval_varnode a d vn) na
  | Auop (_, _) -> na
