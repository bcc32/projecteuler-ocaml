open! Core
open! Import

let problem = Number 7

let main () =
  Sequence.nth_exn Number_theory.Int.primes 10000 (* 0-index *) |> printf "%d\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 104743 |}]
;;

include (val Solution.make ~problem ~main)
