(** Prime numbers, integer and modular arithmetic, factorizations, etc. *)

open! Core
open! Import

module type S = Number_theory_intf.S

module Make (Int : Int_intf.S_unbounded) : S with type integer = Int.t

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

(** [addition_chain_pow ~one ~mul b e] performs
    {{:https://en.wikipedia.org/wiki/Addition-chain_exponentiation} addition-chain
    exponentiation} to calculate [b^e] using as few multiplications as possible.

    Currently supports exponents up to and including 32.  Not functorized for performance
    reasons. *)
val addition_chain_pow_gen : one:'a -> mul:('a -> 'a -> 'a) -> 'a -> int -> 'a

module Int : S with type integer = int
module Bigint : S with type integer = Bigint.t
