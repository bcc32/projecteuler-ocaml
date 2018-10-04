open! Core
module Make (Int : Int_intf.S_unbounded) : Number_theory_intf.S with type integer = Int.t
module Int : Number_theory_intf.S with type integer = int
module Bigint : Number_theory_intf.S with type integer = Bigint.t

val prime_sieve : int -> bool array
val multinomial : int list -> int

(** [factorial_prime_factor n] returns the prime factorization of [n!], but
    without actually calculating the factorial. *)
val factorial_prime_factor : int -> (int * int) list
