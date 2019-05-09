open! Core
open! Import

(* If [p = 2], then no two consecutive terms of the sequence [n^2+m] can be
   divisible by [p], since [n^2] alternates between odd and even.

   Lemma (1): given an odd prime [p = 2a+1], [p] divides two consecutive terms
   of the sequence [n^2+m] iff [p] divides [a^2 + m].

   Proof:

   First, observe that [b^2+m = (-b)^2+m = (p-b)^2+m], mod [p].

   Consider two consecutive terms [b^2+m, (b+1)^2+m], where [b < a].  Their
   difference is [2b+1 < p], so it is impossible for [b^2+m = (b+1)^2+m] mod
   [p].  We also know that [a^2+m = (-a)^2+m = (a+1)^2+m], mod [p].  Therefore,
   the only pair of consecutive equal (mod p) terms can be [a^2 + m] and
   [(a+1)^2 + m].

   If [p] divides [a^2 + m], then it also divides [(a+1)^2 + m], so we only have
   to check [p | a^2 + m] to see if there are two consecutive terms in the
   sequence divisible by [p].

   ∎ *)

(* Find the largest prime that divides two consecutive elements of the sequence
   [1^2+m, 2^2+m, ...].

   Manual inspection reveals that the pattern appears to be the prime factors of
   [4m + 1].

   Lemma (2): letting [p = 2a+1], [p] divides two consecutive terms of the
   sequence [1^2+m, 2^2 + m, ...] iff [p] divides [4m + 1].

   Proof:

   Per lemma (1), [p] divides two consecutive terms of the sequence iff [p]
   divides [a^2 + m].  Equivalently, [a^2 + m = 0] mod [p].

   Rearrange algebraically to get [m = -a^2] mod [p].  This equation is
   equivalent to:

   [4m + 1 = -4a^2 + 1 = (1 - 2a) (1 + 2a) = 0] mod [(1 + 2a)].

   ∎ *)
let find_largest_prime m =
  Number_theory.Int.prime_factor ((4 * m) + 1) |> List.last_exn |> fst
;;

let p k = find_largest_prime (k * k)
let modulus = 1_000_000_000_000_000_000

let sum_p ~max_k =
  let sum = ref 0 in
  for k = 1 to max_k do
    if debug && k mod 10_000 = 0 then Debug.eprint_s [%message (k : int)];
    sum := !sum + p k;
    sum := !sum mod modulus
  done;
  !sum
;;

let main () = sum_p ~max_k:10_000_000 |> printf "%d\n"

include (val Solution.make ~problem:(Number 659) ~main)
