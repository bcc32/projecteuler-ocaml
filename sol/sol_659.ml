open! Core
open! Import

(* If [p = 2], then no two consecutive terms of the sequence [n^2+m] can be divisible by
   [p], since [n^2] alternates between odd and even.

   Given an odd prime [p = 2a+1], [p] divides two consecutive terms of the sequence
   [n^2+m] iff [p] divides [a^2 + m].

   Proof:

   First, observe that [b^2+m = (-b)^2+m = (p-b)^2+m], mod [p].

   Consider two consecutive terms [b^2+m, (b+1)^2+m], where [b < a].  Their difference is
   [2b+1 < p], so it is impossible for [b^2+m = (b+1)^2+m] mod [p].  We also know that
   [a^2+m = (-a)^2+m = (a+1)^2+m], mod [p].  Therefore, the only pair of consecutive equal
   (mod p) terms can be [a^2 + m] and [(a+1)^2 + m].

   If [p] divides [a^2 + m], then it also divides [(a+1)^2 + m], so we only have to check
   [p | a^2 + m] to see if there are two consecutive terms in the sequence divisible by
   [p]. *)

let primes =
  Number_theory.Int.primes
  |> Fn.flip Sequence.drop_eagerly 1
  |> Fn.flip Sequence.take 10_000
  |> Sequence.to_list
;;

(* Find the largest prime that divides two consecutive elements of the sequence [1^2+m,
   2^2+m, ...].

   Manual inspection reveals that the pattern appears to be the prime factors of [4m + 1].
*)
let find_largest_prime m =
  let primes =
    primes
    |> List.filter ~f:(fun p ->
      let k = p / 2 in
      ((k * k) + m) % p = 0)
  in
  print_s [%message (m : int) (primes : int list)]
;;

let main () =
  find_largest_prime 3;
  for i = 1 to 20 do
    find_largest_prime (i * i)
  done
;;

include (val Solution.make ~problem:(Number 659) ~main)
