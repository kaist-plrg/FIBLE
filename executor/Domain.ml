open DS;;

module UJumpD = struct
  type t = (LocSetOption.t) LocBotMap.t
  let join (l1: t) (l2: t): t = LocBotMap.mapjoin LocSetOption.join l1 l2
  let ole (l1: t) (l2: t): bool = LocBotMap.mapole LocSetOption.ole l1 l2
end

module JumpD = struct
  type t = LocSet.t LocBotMap.t
  let join (l1: t) (l2: t): t = LocBotMap.mapjoin LocSet.join l1 l2
  let ole (l1: t) (l2: t): bool = LocBotMap.mapole LocSet.ole l1 l2
end

module BasicBlockD = struct
   type t = (LocSet.t * LocSet.t)
    let join (l1: t) (l2: t): t = (LocSet.join (fst l1) (fst l2), LocSet.join (snd l1) (snd l2))
    let ole (l1: t) (l2: t): bool = (LocSet.ole (fst l1) (fst l2)) && (LocSet.ole (snd l1) (snd l2))
end

module ContourD = struct
  type t = {
    unsound_jump: UJumpD.t;
    basic_block: BasicBlockD.t
    }

  let join (c1: t) (c2: t): t = {
    unsound_jump = UJumpD.join c1.unsound_jump c2.unsound_jump;
    basic_block = BasicBlockD.join c1.basic_block c2.basic_block
  }

  let ole (c1: t) (c2: t): bool = (UJumpD.ole c1.unsound_jump c2.unsound_jump) && (BasicBlockD.ole c1.basic_block c2.basic_block)
end

module FSAbsD = struct
  type t = AbsState.t LocBotMap.t
  let join (l1: t) (l2: t): t = LocBotMap.mapjoin AbsState.join l1 l2
  let ole (l1: t) (l2: t): bool = LocBotMap.mapole AbsState.ole l1 l2
end 