
type cbool = { cbool : 'a. 'a -> 'a -> 'a } ;;

let ctrue: cbool  = { cbool = fun a _b -> a } ;;
let cfalse: cbool = { cbool = fun _a b -> b } ;;

let cbool_of_bool cond = if cond then ctrue else cfalse

(* zamiast true i false może być T: Eq *)
let bool_of_cbool cb = cb.cbool true false = ctrue.cbool true false

let cand lhs rhs = 
  if bool_of_cbool lhs && bool_of_cbool rhs then
    ctrue
  else
    cfalse

let cor lhs rhs = 
  if bool_of_cbool lhs || bool_of_cbool rhs then
    ctrue
  else
    cfalse


type cnum = { cnum: 'a. ('a -> 'a) -> 'a -> 'a } ;;

let inc x = x + 1 ;;
let repeat f n = if n > 0 then fun x -> f (f x) else f;;

let zero = { cnum = fun f x -> f x } ;;

let succ number = { cnum = fun f x -> number.cnum f (f x) } ;;

let cnum_of_int int = repeat succ int zero ;;

let int_of_cnum cnum = (cnum.cnum inc 0) - 1;;

let add lhs rhs = 
  let fold_count = lhs.cnum inc 0 + rhs.cnum inc 0
in 
  print_endline (string_of_int fold_count);
  repeat succ fold_count zero

let mul lhs rhs = repeat 