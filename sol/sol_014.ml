open! Core
open! Import

let limit = 1_000_000
let collatz n = if n mod 2 = 0 then n / 2 else (3 * n) + 1

let rec collatz_length =
  let cache = Array.create (-1) ~len:(limit + 1) in
  cache.(1) <- 1;
  fun n ->
    if n <= limit && cache.(n) <> -1
    then cache.(n)
    else (
      let length = collatz_length (collatz n) + 1 in
      if n <= limit then cache.(n) <- length;
      length)
;;

let main () =
  Sequence.range ~stop:`inclusive 1 limit
  |> Sequence.max_elt ~compare:(fun a b ->
    Int.compare (collatz_length a) (collatz_length b))
  |> Option.value_exn
  |> printf "%d\n"
;;

(* 72ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 837799 |}]
;;

include (val Solution.make ~problem:(Number 14) ~main)
