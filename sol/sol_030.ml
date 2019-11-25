open! Core
open! Import

(* Derived by observing that 9^5 * n < 10^(n - 1) for all n > 6. *)
let max_digits = 6
let limit = Int.pow 10 max_digits

(* optimized for constant exponent

   See https://en.wikipedia.org/wiki/Addition-chain_exponentiation *)
let[@inline always] fifth_power n =
  let nn = n * n in
  nn * nn * n
;;

let sum_fifth_of_digits n =
  let rec loop n ac = if n = 0 then ac else loop (n / 10) (ac + fifth_power (n mod 10)) in
  loop n 0
;;

let main () =
  let sum = ref 0 in
  for n = 2 to limit - 1 do
    if n = sum_fifth_of_digits n then sum := !sum + n
  done;
  printf "%d\n" !sum
;;

(* 34.65ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 443839 |}]
;;

include (val Solution.make ~problem:(Number 30) ~main)
