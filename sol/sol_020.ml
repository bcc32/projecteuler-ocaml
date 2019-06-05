open! Core
open! Import

let main () =
  Bigint.of_int 100
  |> Number_theory.Bigint.factorial
  |> Number_theory.Bigint.As_base10.sum (module Bigint) ~f:Fn.id
  |> printf !"%{Bigint}\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 648 |}]
;;

include (val Solution.make ~problem:(Number 20) ~main)
