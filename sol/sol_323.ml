open! Core
open! Import

let word_size = 32

(* probability of transitioning from [a] 1 bits to [b] 1 bits *)
let p_transition a b =
  assert (a <= b);
  assert (a < 32);
  assert (b <= 32);
  let zero_bits = word_size - a in
  let diff = b - a in
  float (Number_theory.Int.binomial zero_bits diff) /. (2. ** float zero_bits)
;;

let expectation_to_32 =
  Memo.recursive
    (module Int)
    (fun expectation_to_32 n ->
      if n = 32
      then 0.0
      else (
        let e = ref 1. in
        (* - E[n] = 1 + P[n] E[n] + P[n + 1]E[n + 1] + ... + P[32]E[32]
           - E[n] = (1 + P[n + 1]E[n + 1] + ... + P[32]E[32]) / (1 - P[n]) *)
        for i = n + 1 to 32 do
          e := !e +. (p_transition n i *. expectation_to_32 i)
        done;
        !e /. (1. -. p_transition n n)))
;;

let main () = printf "%.10f\n" @@ expectation_to_32 0

(* 0.166ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 6.3551758451 |}]
;;

include (val Solution.make ~problem:(Number 323) ~main)
