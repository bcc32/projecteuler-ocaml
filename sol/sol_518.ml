open! Core
open! Import

let iter_triples ~limit ~f =
  let primes = Number_theory.prime_sieve limit in
  for a = 2 to limit / 4 do
    if primes.(a)
    then (
      (* TODO Non-integer ratios*)
      let rec loop ratio =
        let b = (ratio * (a + 1)) - 1 in
        let c = (ratio * (b + 1)) - 1 in
        if c > limit
        then ()
        else (
          if primes.(b) && primes.(c) then f a b c;
          loop (ratio + 1))
      in
      loop 2)
  done
;;

let limit = 100

let main () =
  let sum = ref 0 in
  iter_triples ~limit ~f:(fun a b c ->
    printf "%d %d %d\n" a b c;
    sum := !sum + a + b + c);
  printf "%d\n" !sum
;;

let%expect_test _ =
  main ();
  [%expect
    {|
      2 5 11
      2 11 47
      5 11 23
      5 17 53
      7 23 71
      11 23 47
      374 |}]
;;

include (val Solution.make ~problem:(Number 518) ~main)
