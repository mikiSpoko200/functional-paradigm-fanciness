(* Zadanie 1 *)

fun x -> x ;;

fun x -> x + 0 ;;

let compose outer inner = fun x -> outer (inner x) ;;
(* note wiązanie aplikacji wymusza nawiasy *)

let ignore_second x _ = x ;;

let first_if_equal_other_second a b = if a = b then a else b ;;

let mysterious = let rec inner x = match x with
    | [] -> inner []
    | a :: t -> a
in inner []

(* Zadanie 2 *)

let mysterious_return x = mysterious ;;

(* Zadanie 3 *)

let hd stream = stream 0 ;;
let tl stream = fun index -> stream (index + 1) ;;

let add stream const = fun index -> stream index + const ;;

let map stream f = fun index -> f (stream index) ;;

(* map może zbudować add *)
let map_add stream const = map stream (fun x -> x + const);;

let map2 f s1 s2 = fun index -> f (s1 index) (s2 index) ;;

let replace n value stream = fun index -> if index = n then value else stream index ;;

let take_every n s = fun index -> s (n * index) ;;

(* Zadanie 5 *)

let tabulate stream ?(start_index=0) end_index =
    let rec inner acc index =
        if start_index > index then
            acc
        else
            inner (stream index :: acc) (index - 1)
    in inner [] end_index
;;

(* Zadanie 6 *)

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

(* Zadanie 7 *)

(* W tym konkretnym wywołaniu ponieważ nie skłądamy funkcji f może być 'a -> 'b. *)
let inc x = x + 1 ;;
let zero f x = f x ;;

let typed_zero f x = let result = f x in if result = x then result else result ;;

let succ number = fun f x -> number f (f x) ;;

let two = succ (succ zero) inc 0 ;;

(* Number może zmienić typ wartości zwracanej -- możne zmienic ifem *)
let int_of_cnum number = number inc 0 ;;
let typed_int_of_cnum number = let result = number inc 0 in if result = 0 then result else result ;;

(* Zadanie 8 *)

type cbool = { cbool: 'a. 'a -> 'a -> 'a } ;;
type cnum = { cnum: 'a. ('a -> 'a) -> 'a -> 'a } ;;


