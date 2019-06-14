open! Core
open! Import

let main () =
  Number_theory.Int.fibonacci
  |> Sequence.take_while ~f:(fun x -> x <= 4000000)
  |> Sequence.filter ~f:(fun x -> x mod 2 = 0)
  |> Sequence.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

(* 6us *)
let%expect_test "answer" =
  main ();
  [%expect {| 4613732 |}]
;;

include (val Solution.make ~problem:(Number 2) ~main)
