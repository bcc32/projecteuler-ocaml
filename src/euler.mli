open Core.Std
open Bignum.Std

(* Utility *)
val run_length_encode : 'a list -> ('a * int) list
val is_palindrome : 'a list -> equal:('a -> 'a -> bool) -> bool
val permutations : cmp:('a -> 'a -> int) -> 'a list -> 'a list Sequence.t

(* Number Theory *)
val digits_of_string : string -> int list
val factor : int -> int list
val prime_factor : int -> (int * int) list
val divisors : int -> int list
val num_divisors : int -> int

(* Geometry *)
val is_pythagorean_triple : int -> int -> int -> bool

module Number_theory : sig
  module type S = sig
    type int
    val range
      :  ?stride:int
      -> ?start:[ `inclusive | `exclusive ]
      -> ?stop:[ `exclusive | `inclusive ]
      -> int -> int -> int Sequence.t
    val digits_of_int : ?base:int -> int -> int list
    val int_of_digits : ?base:int -> int Sequence.t -> int
    val sum_digits : ?base:int -> int -> int
    val factorial : int -> int
    val is_prime : int -> bool
    val next_probable_prime : int -> int
    val next_prime : int -> int
    val primes : int Sequence.t
    val fibonacci : int Sequence.t
    val binomial : int -> int -> int
    val natural_numbers : ?init:int -> unit -> int Sequence.t
  end

  module Make(Int : Int_intf.S) : S
end

module Int    : Number_theory.S with type int = int
module Bigint : Number_theory.S with type int = Bigint.t
