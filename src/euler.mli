open Core.Std
open Bignum.Std

(* Utility *)
val run_length_encode : 'a list -> ('a * int) list
val is_palindrome : 'a list -> equal:('a -> 'a -> bool) -> bool

(* Number Theory *)
val digits_of_int : ?base:int -> int -> int list
val digits_of_string : string -> int list
val sum_digits : ?base:Bigint.t -> Bigint.t -> Bigint.t
val factorial : int -> Bigint.t
val is_prime : int -> bool
val next_probable_prime : int -> int
val next_prime : int -> int
val primes : int Sequence.t
val factor : int -> int list
val prime_factor : int -> (int * int) list
val divisors : int -> int list
val num_divisors : int -> int
val fibonacci : Bigint.t Sequence.t
val binomial : int -> int -> Bigint.t
val natural_numbers : ?init:int -> unit -> int Sequence.t

(* Geometry *)
val is_pythagorean_triple : int -> int -> int -> bool
