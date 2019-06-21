open! Core
open! Import

module type As_digits_one_direction = sig
  type integer

  val base : integer
  val of_list : integer list -> integer
  val of_array : integer array -> integer
  val of_sequence : integer Sequence.t -> integer
  val to_sequence : integer -> integer Sequence.t
  val append : integer -> integer -> integer

  include Container.S0 with type t := integer with type elt := integer
end

(** [As_digits] provides a view of an integer as a sequence of digits. *)
module type As_digits = sig
  type integer

  (** [rev n] reverses the digits of [n].

      Notably, this function doesn't round-trip, e.g., [10 -> 01 -> 1]. *)
  val rev : integer -> integer

  (** Most significant digit to least significant digit. *)
  module Left_to_right : As_digits_one_direction with type integer := integer

  (** Least significant digit to most significant digit. *)
  module Right_to_left : As_digits_one_direction with type integer := integer

  (** Equivalent to Left_to_right. *)
  include
    As_digits_one_direction with type integer := integer
end

module type S = sig
  type integer

  val range
    :  ?stride:integer
    -> ?start:[ `inclusive | `exclusive ]
    -> ?stop:[ `exclusive | `inclusive ]
    -> integer
    -> integer
    -> integer Sequence.t

  (*_ FIXME Some of this doesn't really belong in "number theory", maybe separate
    out the combinatorics, digits, arithmetic, etc. *)

  (** {1 Digits} *)

  module As_digits (M : sig
      val base : integer
    end) : As_digits with type integer = integer

  module As_base10 : As_digits with type integer = integer

  (** {1 Combinatorics} *)

  (** [n!] *)
  val factorial : integer -> integer

  (** C(n, r) is the number of ways to choose [r] unordered elements from a set
      of [n] elements. *)
  val binomial : integer -> integer -> integer

  (** {1 Primes} *)

  val is_prime : integer -> bool
  val next_probable_prime : integer -> integer
  val next_prime : integer -> integer
  val primes : integer Sequence.t

  (** {1 Factors} *)

  (** [gcd n m] is the greatest common divisor of [n] and [m]. *)
  val gcd : integer -> integer -> integer

  (** [lcm n m] is the least common multiple of [n] and [m]. *)
  val lcm : integer -> integer -> integer

  (** [factor n] returns a non-descending list of prime factors of [n], with
      repeated entries corresponding to multiplicity. *)
  val factor : integer -> integer list

  (** [prime_factor n] returns a non-descending list of prime factors of [n]
      with their multiplicities. *)
  val prime_factor : integer -> (integer * int) list

  (** [divisors n] returns all of the divisors of [n], not necessarily in sorted order. *)
  val divisors : integer -> integer list

  (** [num_divisors n] returns the number of divisors of [n] *)
  val num_divisors : integer -> integer

  (** [totient n] calculates Euler's totient function phi(n), the number of
      relatively prime integers [m] where [1 <= m < n]. *)
  val totient : integer -> integer

  (** {1 Modular arithmetic} *)

  (** [powmod a b ~modulus] returns [a^b mod modulus], without overflowing *)
  val powmod : integer -> integer -> modulus:integer -> integer

  (** [bezout a b] returns [(s, t, g)] such that [g = gcd(a, b)] and [s * a + t
   * b = g] *)
  val bezout : integer -> integer -> integer * integer * integer

  (** [chinese_remainder_theorem [(r_1, m_1); ...]] returns [(x, m)] such that
      [x % m_1 = r_1], and so on. All of [m_i] must be pairwise coprime. [m] is
      the product of [m_i]. *)
  val chinese_remainder_theorem : (integer * integer) list -> integer * integer

  (** {1 Miscellaneous interesting sequences} *)

  (** 0, 1, 1, 2, 3, 5, ... *)
  val fibonacci : integer Sequence.t

  (** calculates F(n) in O(log(n)) time, where F(0) = 0, F(1) = 1, ... *)
  val fast_fibonacci : integer -> integer

  (** @param init default is 0 *)
  val natural_numbers : ?init:integer -> unit -> integer Sequence.t

  (** {1 Square roots} *)

  (** [isqrt n] returns the floor of [sqrt n], but using purely integer
      arithmetic *)
  val isqrt : integer -> integer

  val is_perfect_square : integer -> bool

  (*_ TODO This probably doesn't belong in the Number_theory module.  Consider moving it
    into its own module, along with [isqrt] and [is_perfect_square]. *)

  (** {1 Optimized basic arithmetic} *)

  val addition_chain_pow : integer -> int -> integer
end

module type Number_theory = sig
  (** Prime numbers, integer and modular arithmetic, factorizations, etc. *)

  module type As_digits = As_digits
  module type As_digits_one_direction = As_digits_one_direction
  module type S = S

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
end
