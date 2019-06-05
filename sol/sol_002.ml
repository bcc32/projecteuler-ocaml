open! Core
open! Import

let fibs n =
  let rec iter a b acc = if b > n then acc else iter b (a + b) (b :: acc) in
  iter 0 1 [] |> List.rev
;;

let main () =
  fibs 4000000
  |> List.filter ~f:(fun x -> x mod 2 = 0)
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 4613732 |}]
;;

include (val Solution.make ~problem:(Number 2) ~main)
