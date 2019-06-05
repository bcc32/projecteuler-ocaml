open! Core
open! Import

let main () =
  let n = Bigint.of_int 40 in
  let r = Bigint.of_int 20 in
  Number_theory.Bigint.binomial n r |> printf !"%{Bigint}\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 137846528820 |}]
;;

include (val Solution.make ~problem:(Number 15) ~main)
