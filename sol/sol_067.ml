open! Core
open! Import

let main () =
  Problem_067.data
  |> Parse.space_separated_grid ~conv:Int.of_string
  |> Sol_018.max_sum_exn
  |> printf "%d\n"
;;

(* 0.903ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 7273 |}]
;;

include (val Solution.make ~problem:(Number 67) ~main)
