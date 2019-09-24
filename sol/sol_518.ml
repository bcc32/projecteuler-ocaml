open! Core
open! Import

let iter_triples ~limit ~f =
  let primes = Number_theory.prime_sieve limit in
  for a = 2 to limit do
    if primes.(a)
    then
      for b = a + 1 to limit do
        if primes.(b)
        then (
          let numer = (b + 1) * (b + 1) in
          let denom = a + 1 in
          if numer % denom = 0
          then (
            let c = (numer / denom) - 1 in
            if c <= limit && primes.(c) then f a b c))
      done
  done
;;

let sum_triples ~limit ~print =
  let sum = ref 0 in
  iter_triples ~limit ~f:(fun a b c ->
    if print then printf "%d %d %d\n" a b c;
    sum := !sum + a + b + c);
  !sum
;;

let%expect_test "example" =
  sum_triples ~limit:100 ~print:true |> printf "%d\n";
  [%expect
    {|
    2 5 11
    2 11 47
    5 11 23
    5 17 53
    7 11 17
    7 23 71
    11 23 47
    17 23 31
    17 41 97
    31 47 71
    71 83 97
    1035 |}]
;;

let main () = sum_triples ~limit:100_000_000 ~print:false |> printf "%d\n"

include (val Solution.make ~problem:(Number 518) ~main)
