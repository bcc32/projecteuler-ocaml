open! Core
open! Import

type answer =
  { mutable a : int
  ; mutable b : int
  ; mutable number_of_primes : int
  }
[@@deriving sexp]

let is_prime = lazy (Number_theory.prime_sieve 20_000)
let largest_q = ref 0

let count_primes a b =
  let rec loop n count =
    let q = (n * n) + (a * n) + b in
    if q > !largest_q then largest_q := q;
    if q >= 0 && (force is_prime).(q) then loop (n + 1) (count + 1) else count
  in
  loop 0 0
;;

let main () =
  let answer = { a = 0; b = 0; number_of_primes = 0 } in
  for a = -999 to 999 do
    for b = -1000 to 1000 do
      let primes = count_primes a b in
      if primes > answer.number_of_primes
      then (
        answer.a <- a;
        answer.b <- b;
        answer.number_of_primes <- primes)
    done
  done;
  if debug then Debug.eprint_s [%message "" (answer : answer) (!largest_q : int)];
  printf "%d\n" (answer.a * answer.b)
;;

(* 50ms *)
let%expect_test "answer" =
  main ();
  [%expect {| -59231 |}]
;;

include (val Solution.make ~problem:(Number 27) ~main)
