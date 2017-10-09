open! Core
open! Import

module M = struct
  let problem = `Number 429

  (* The unitary divisors of [n] are [p_1^a_1 * ... * p_k^a_k] where [p_1, ...,
     p_k] are any subset of the prime factors of [n].

     Induct on [k]. Let [S_k(n)] denote the sum of the squares of unitary
     divisors corresponding to primes [1..k].

     For [k = 0], [S_0(n) = 1^2 = 1].

     For [k + 1], we have:

     {[
       S_{k + 1}(n) = (p_1^a_1 * ... * p_k^a_k)^2 + ...
                    = S_k(n) + S_k(n) * (p_{k+1}^{a_{k+1}})^2
     ]}

     For each term above, a new copy has p_{k+1}^{a_{k+1}} since we can factor
     out the common new prime.

     Thus, once we have the prime factorization of [n], we can use an easy
     linear recurrence to calculate [S(n)]. *)

  let modulus = 1_000_000_009

  let main () =
    let factors = Number_theory.factorial_prime_factor 100_000_000 in
    let s = ref 1 in
    List.iter factors ~f:(fun (p, a) ->
      s := !s * (1 + Number_theory.Int.powmod p (2 * a) ~modulus) mod modulus);
    printf "%d\n" !s
  ;;
  (* 98792821 9.3s *)
end

include Solution.Make(M)
