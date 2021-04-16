module Tests.GDTLC.Shared where


import GDTLC.Data.Term
import GDTLC.Data.Name



var :: String -> ITerm
var x = Free (Global x)

var' :: String -> CTerm
var' = Inf . var