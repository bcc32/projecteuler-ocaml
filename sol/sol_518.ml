open! Core
open! Import

exception C_greater_than_limit [@@deriving sexp_of]

let iter_triples ~limit ~f =
  let primes = Number_theory.prime_sieve limit in
  for a = 2 to limit do
    if primes.(a)
    then (
      try
        for b = a + 1 to limit do
          if primes.(b)
          then (
            let numer = (b + 1) * (b + 1) in
            let denom = a + 1 in
            if numer % denom = 0
            then (
              let c = (numer / denom) - 1 in
              if c > limit then Exn.raise_without_backtrace C_greater_than_limit;
              if primes.(c) then f a b c))
        done
      with
      | C_greater_than_limit -> ())
  done
;;

let sum_triples ~limit =
  let sum = ref 0 in
  let triples = ref 0 in
  iter_triples ~limit ~f:(fun a b c ->
    if debug && !triples % 1_000 = 0 then Debug.eprintf "%d %d %d" a b c;
    sum := !sum + a + b + c;
    incr triples);
  !sum
;;

let%expect_test "example" =
  sum_triples ~limit:100 |> printf "%d\n";
  [%expect {| 1035 |}]
;;

let main () = sum_triples ~limit:100_000_000 |> printf "%d\n"

include (val Solution.make ~problem:(Number 518) ~main)
