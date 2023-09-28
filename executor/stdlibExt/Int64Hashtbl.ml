include HashtblExt.Make(struct
type t = int64
let equal = (=)
let hash = HashtblExt.hash
end)