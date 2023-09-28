module Make(A: DomainSpec.JoinSemiLatitce) = struct
  include BotMapD.Make(Basic.Loc)(A)
end