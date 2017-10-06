open Core
open Bignum.Std

module Solution = Solution

(* Utility *)
val run_length_encode : 'a list -> ('a * int) list
val is_palindrome : 'a list -> equal:('a -> 'a -> bool) -> bool
val permutations : cmp:('a -> 'a -> int) -> 'a list -> 'a list Sequence.t
val digits_of_string : string -> int list

(* Geometry *)
val is_pythagorean_triple : int -> int -> int -> bool

(* Number Theory *)
val prime_sieve : int -> bool array

module Int    : Number_theory_intf.S with type integer = int
module Bigint : Number_theory_intf.S with type integer = Bigint.t

module Float  : Numerics_intf.S with type real = float
module Bignum : Numerics_intf.S with type real = Bignum.t
