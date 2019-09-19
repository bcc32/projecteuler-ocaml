open! Core
open! Import

let%bench_fun "prime_sieve(10^6)" =
  Gc.disable_compaction ~allocation_policy:`Don't_change ();
  fun () -> Number_theory.prime_sieve 1_000_000
;;
