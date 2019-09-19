open! Core
open! Import

let%bench_fun ("is_prime cached"[@indexed n = [ 10; 100; 1_000; 10_000 ]]) =
  ignore (Number_theory.Int.is_prime n : bool);
  fun () -> Number_theory.Int.is_prime n
;;
