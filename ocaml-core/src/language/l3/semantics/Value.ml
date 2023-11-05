module NonNumericValue_ = NonNumericValue
include Common_language.ValueF.Make(NonNumericValue_)

let localP v = NonNum (NonNumericValue_.LocalP v)
let paramP v = NonNum (NonNumericValue_.ParamP v)
let sp v = NonNum (NonNumericValue_.SP v)