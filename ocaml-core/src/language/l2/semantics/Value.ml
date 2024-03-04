module NonNumericValue_ = NonNumericValue
include Common_language.ValueF.Make (NonNumericValue_)

let sp v = NonNum (NonNumericValue_.SP v)
