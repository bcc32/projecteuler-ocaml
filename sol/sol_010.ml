open! Core
open! Import

let main () =
  let limit = 2_000_000 in
  let is_prime = Number_theory.prime_sieve limit in
  let sum = ref 0 in
  for i = 2 to limit do
    if is_prime.(i) then sum := !sum + i
  done;
  printf "%d\n" !sum
;;

let%expect_test "answer" =
  main ();
  [%expect {| 142913828922 |}]
;;

include (val Solution.make ~problem:(Number 10) ~main)
