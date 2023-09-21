open DS;;

module RegTopMapD = struct
  type t = AbsVal.t Int64Map.t
  let join a b = Int64Map.merge (fun _ a b -> match a,b with
    | Some a, Some b -> Some (AbsVal.join a b)
    | _, _ -> None
  ) a b
  let top = Int64Map.empty

  let ole (m1: t) (m2: t): bool =
    Int64Map.fold (fun k v2 acc -> match Int64Map.find_opt k m1 with
     | Some v1 -> acc && (AbsVal.ole v1 v2)
     | None -> acc && (AbsVal.ole AbsVal.top v2)
    ) m2 true
  let ole _ _ = true
end

type t = {
  unique_regs: RegTopMapD.t;
  general_regs: RegTopMapD.t
}
let join a b = {
  unique_regs = RegTopMapD.join a.unique_regs b.unique_regs;
  general_regs = RegTopMapD.join a.general_regs b.general_regs
}
let top = {
  unique_regs = RegTopMapD.top;
  general_regs = RegTopMapD.top
}

let ole a b = RegTopMapD.ole a.unique_regs b.unique_regs && RegTopMapD.ole a.general_regs b.general_regs