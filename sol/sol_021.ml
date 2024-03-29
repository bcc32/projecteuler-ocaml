open! Core
open! Import

let sum_proper_divisors =
  Memo.simple
    (module Int)
    (fun n ->
      let sd = List.sum (module Int) (Number_theory.Int.divisors n) ~f:Fn.id in
      sd - n)
;;

let amicable n =
  let sd = sum_proper_divisors n in
  sd <> n && sum_proper_divisors sd = n
;;

let main () =
  Sequence.range 2 10_000
  |> Sequence.filter ~f:amicable
  |> Sequence.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

(* 20.397ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 31626 |}]
;;

include (val Solution.make ~problem:(Number 21) ~main)
