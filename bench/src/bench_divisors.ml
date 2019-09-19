open! Core
open! Import

let%bench ("Euler.Int.divisors"[@indexed n = [ 17; 60; 100003; 120000; 600851475143 ]]) =
  Number_theory.Int.divisors n
;;
