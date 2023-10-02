module Make(A: DomainSpec.JoinSemiLatitce) = struct
  include BotMapD.Make(Basic.Loc)(A)
end

module Make_Mut(A: DomainSpec.JoinSemiLatitce) = struct
  include BotMapD.Make_Mut(Basic.Loc)(A)
end