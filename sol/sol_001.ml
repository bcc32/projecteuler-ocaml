open! Core
open! Import

let main () =
  List.range 1 1000
  |> List.filter ~f:(fun x -> x mod 3 = 0 || x mod 5 = 0)
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 233168 |}]
;;

include (val Solution.make ~problem:(Number 1) ~main)
