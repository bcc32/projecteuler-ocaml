open! Core
open! Import

module type S = sig
  type integer

  val range :
    ?stride:integer
    -> ?start:[`inclusive | `exclusive]
    -> ?stop:[`exclusive | `inclusive]
    -> integer
    -> integer
    -> integer Sequence.t
  (* FIXME Some of this doesn't really belong in "number theory", maybe separate
     out the combinatorics, digits, arithmetic, etc. *)

  (* TODO rename to to_digits, of_digits *)

  (** {1 Digits} *)

  (** [fold_digits ?base n ~init ~f] folds over the digits of [n] from least significant
      to most significant. *)
  val fold_digits : ?base:integer -> integer -> init:'a -> f:('a -> integer -> 'a) -> 'a

  (** least significant to most significant *)
  val iter_digits : ?base:integer -> integer -> f:(integer -> unit) -> unit

  (** left-to-right *)
  val digits_of_int : ?base:integer -> integer -> integer list

  (** left-to-right *)
  val int_of_digits : ?base:integer -> integer Sequence.t -> integer

  val sum_digits : ?base:integer -> integer -> integer

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
end
