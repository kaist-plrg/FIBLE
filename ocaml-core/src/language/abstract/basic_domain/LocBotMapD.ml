module Make (A : DomainSpec.JoinSemiLatitce) = struct
  include BotMapD.Make (Common.Loc) (A)
end

module Make_Mut (A : DomainSpec.JoinSemiLatitce) = struct
  include BotMapD.Make_Mut (Common.Loc) (A)
end
