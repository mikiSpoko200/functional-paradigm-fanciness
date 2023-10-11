(* W tym konkretnym wywołaniu ponieważ nie skłądamy funkcji f może być 'a -> 'b. *)
let inc x = x + 1 ;;
let zero f x = f x ;;

let typed_zero f x = let result = f x in if result = x then result else result ;;

let succ number = fun f x -> number f (f x) ;;

let two = succ (succ zero) inc 0 ;;

(* Number może zmienić typ wartości zwracanej -- możne zmienic ifem *)
let int_of_cnum number = number inc 0 ;;
let typed_int_of_cnum number = let result = number inc 0 in if result = 0 then result else result ;;