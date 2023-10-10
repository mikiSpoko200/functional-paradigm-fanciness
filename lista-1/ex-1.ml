fun x -> x;;

(* ? Napisz wyrażenie, którego wartością też jest funkcja identycznościowa, ale które ma typ int -> int *)
fun x -> x + 0

let compose outer inner x = outer (inner x)

(* note wiązanie aplikacji wymusza nawiasy *)

let ignore_second x _ = x
let first_if_equal_other_second a b = if a = b then a else b

let mysterious =
  let rec inner x = match x with [] -> inner [] | a :: t -> a in
  inner []
