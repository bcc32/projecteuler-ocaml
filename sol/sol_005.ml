open! Core
open! Import

let main () =
  List.range ~stop:`inclusive 1 20
  |> List.fold ~init:1 ~f:Number_theory.Int.lcm
  |> printf "%d\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 232792560 |}]
;;

include (val Solution.make ~problem:(Number 5) ~main)
