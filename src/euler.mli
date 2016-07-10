open Core.Std

(* Utility *)
val run_length_encode : 'a list -> ('a * int) list

(* Number Theory *)
val digits_of_int : int -> int list
val digits_of_string : string -> int list
val is_prime : int -> bool
val next_probable_prime : int -> int
val next_prime : int -> int
val primes : int Sequence.t
val factor : int -> int list
val prime_factor : int -> (int * int) list
val divisors : int -> int list
val num_divisors : int -> int

(* Geometry *)
val is_pythagorean_triple : int -> int -> int -> bool
