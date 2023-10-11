let ctrue a b = if true then a else b ;;
let cfalse a b = if false then a else b ;;

let ctest cond a b = cond a b = ctrue a b ;;

let cand lhs rhs = 
    fun a b ->
        if ctest lhs a b && ctest rhs a b then
            ctrue a b
        else
            cfalse a b

let cor lhs rhs = 
    fun a b ->
        if ctest lhs a b || ctest rhs a b then
            ctrue a b
        else
            cfalse a b

let cbool_of_bool cond = if cond then ctrue else cfalse

let bool_of_cbool (ccond: 'a -> 'a -> 'a) = ctest ccond true  false 