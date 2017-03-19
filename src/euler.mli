open Core
open Bignum.Std

(* Utility *)
val run_length_encode : 'a list -> ('a * int) list
val is_palindrome : 'a list -> equal:('a -> 'a -> bool) -> bool
val permutations : cmp:('a -> 'a -> int) -> 'a list -> 'a list Sequence.t
val digits_of_string : string -> int list

(* Geometry *)
val is_pythagorean_triple : int -> int -> int -> bool

(* Number Theory *)
val prime_sieve : int -> bool array

module Number_theory : sig
  module type S = sig
    type integer
    val range
      :  ?stride:integer
      -> ?start:[ `inclusive | `exclusive ]
      -> ?stop:[ `exclusive | `inclusive ]
      -> integer -> integer -> integer Sequence.t
    val digits_of_int : ?base:integer -> integer -> integer list
    val int_of_digits : ?base:integer -> integer Sequence.t -> integer
    val sum_digits : ?base:integer -> integer -> integer
    val factorial : integer -> integer
    val is_prime : integer -> bool
    val next_probable_prime : integer -> integer
    val next_prime : integer -> integer
    val primes : integer Sequence.t
    val fibonacci : integer Sequence.t
    val binomial : integer -> integer -> integer
    val natural_numbers : ?init:integer -> unit -> integer Sequence.t
    val factor : integer -> integer list
    val prime_factor : integer -> (integer * int) list
    val divisors : integer -> integer list
    val num_divisors : integer -> integer
    val totient : integer -> integer
  end

  module Make(Int : Int_intf.S) : S
end

module Int    : Number_theory.S with type integer = int
module Bigint : Number_theory.S with type integer = Bigint.t

module Numerics : sig
  module type Real_intf = sig
    type t
    val abs : t -> t
    val (+)   : t -> t -> t
    val (-)   : t -> t -> t
    val ( * ) : t -> t -> t
    val (/)   : t -> t -> t

    val of_int : int -> t

    val sign : t -> Sign.t

    include Comparable.S with type t := t
  end

  module type S = sig
    type real
    val bisect
      :  f : (real -> real)
      -> epsilon : real
      -> low     : real
      -> high    : real
      -> real
    val newton's_method
      :  f  : (real -> real)
      -> f' : (real -> real)
      -> epsilon : real
      -> init    : real
      -> real
  end

  module Make(Real : Real_intf) : S
end

module Float  : Numerics.S with type real = float
module Bignum : Numerics.S with type real = Bignum.t
