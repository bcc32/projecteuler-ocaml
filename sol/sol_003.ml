open! Core
open! Import

let main () = Number_theory.Int.factor 600851475143 |> List.last_exn |> printf "%d\n"

let%expect_test "answer" =
  main ();
  [%expect {| 6857 |}]
;;

include (val Solution.make ~problem:(Number 3) ~main)
