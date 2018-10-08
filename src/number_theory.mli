(** Prime numbers, integer and modular arithmetic, factorizations, etc. *)

open! Core
open! Import
module Make (Int : Int_intf.S_unbounded) : Number_theory_intf.S with type integer = Int.t
module Int : Number_theory_intf.S with type integer = int
module Bigint : Number_theory_intf.S with type integer = Bigint.t

(** [prime_sieve limit] uses the Sieve of Eratosthenes to determine which of
    [[0, n]] are prime.

    @return an array [x] such that [x.(i)] is true iff i is prime. *)
val prime_sieve : int -> bool array

(** [multinomial xs] equals [n! / (x1! * x2! * ... * xk!)] where [n = x1 + x2 +
    ... + xk].  It is equal to the number of ways to arrange [n] items
    consisting of [x] of each of [k] distinct categories of items. *)
val multinomial : int list -> int

(** [factorial_prime_factor n] returns the prime factorization of [n!], but
    without actually calculating the factorial. *)
val factorial_prime_factor : int -> (int * int) list
