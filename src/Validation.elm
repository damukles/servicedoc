module Validation exposing (..)


isValidStringProp : String -> Bool
isValidStringProp =
    (<) 0 << String.length


isValidIdProp : Int -> Bool
isValidIdProp =
    (<) 0
