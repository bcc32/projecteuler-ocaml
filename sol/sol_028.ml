open! Core
open! Import

let main () =
  let i = ref 1 in
  let di = ref 2 in
  let sum = ref 1 in
  for _ = 1 to 500 do
    for _ = 1 to 4 do
      i := !i + !di;
      sum := !sum + !i
    done;
    di := !di + 2
  done;
  printf "%d\n" !sum
;;

(* 8.126us *)
let%expect_test "answer" =
  main ();
  [%expect {| 669171001 |}]
;;

include (val Solution.make ~problem:(Number 28) ~main)
