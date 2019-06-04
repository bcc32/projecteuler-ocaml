open! Core
open! Import

let prime_pi_naive n =
  Sequence.range 2 n ~stop:`inclusive |> Sequence.count ~f:Number_theory.Int.is_prime
;;

let pi_sequences u0 ~is_prime ~prime_pi =
  Sequence.unfold_step ~init:u0 ~f:(fun n ->
    if n < 1
    then Done
    else (
      let next = prime_pi n in
      Yield (n, next)))
  |> Sequence.folding_map ~init:0 ~f:(fun num_non_prime n ->
    let num_non_prime =
      if not (is_prime n) then num_non_prime + 1 else num_non_prime
    in
    num_non_prime, (n, num_non_prime))
;;

let%expect_test "u(10)" =
  pi_sequences 10 ~is_prime:Number_theory.Int.is_prime ~prime_pi:prime_pi_naive
  |> [%sexp_of: (int * int) Sequence.t]
  |> print_s;
  [%expect {| ((10 1) (4 2) (2 2) (1 3)) |}]
;;

let big_p n ( * ) ~is_prime ~prime_pi =
  let num_sequences_by_c = Hashtbl.create (module Int) in
  for u0 = 1 to n do
    if debug && u0 % 100_000 = 0
    then Debug.eprintf !"%{Percent}" (Percent.of_mult (float u0 /. float n));
    pi_sequences u0 ~is_prime ~prime_pi
    |> Fn.flip Sequence.drop_eagerly 1
    |> Sequence.iter ~f:(fun (_, c) -> Hashtbl.incr num_sequences_by_c c)
  done;
  Hashtbl.fold num_sequences_by_c ~init:1 ~f:(fun ~key:_ ~data acc -> acc * data)
;;

let%expect_test "P(10)" =
  big_p 10 ( * ) ~is_prime:Number_theory.Int.is_prime ~prime_pi:prime_pi_naive
  |> [%sexp_of: int]
  |> print_s;
  [%expect {| 648 |}]
;;

let%expect_test "P(100)" =
  big_p 100 ( * ) ~is_prime:Number_theory.Int.is_prime ~prime_pi:prime_pi_naive
  |> [%sexp_of: int]
  |> print_s;
  [%expect {| 31038676032 |}]
;;

let problem = Number 609
let limit = 100_000_000

module Int_with_modulus = struct
  let modulus = 1_000_000_007
  let ( * ) a b = a * b mod modulus
end

let main () =
  let is_prime =
    debug_timing ~here:[%here] ~task:"sieving" Number_theory.prime_sieve limit
  in
  let prime_pi =
    let cumulative_prime_count =
      debug_timing
        ~here:[%here]
        ~task:"cumulative prime count"
        (fun is_prime ->
           Array.folding_map is_prime ~init:0 ~f:(fun num_primes is_prime ->
             let num_primes = if is_prime then num_primes + 1 else num_primes in
             num_primes, num_primes))
        is_prime
    in
    fun n -> cumulative_prime_count.(n)
  in
  big_p limit Int_with_modulus.( * ) ~is_prime:(Array.get is_prime) ~prime_pi
  |> printf "%d\n"
;;

(* 172023848
   1m24.610572005s *)

include (val Solution.make ~problem ~main)
