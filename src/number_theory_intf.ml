open! Core
open! Import

module type S = sig
  type integer

  val range
    :  ?stride:integer
    -> ?start:[`inclusive | `exclusive]
    -> ?stop:[`exclusive | `inclusive]
    -> integer
    -> integer
    -> integer Sequence.t

  (** [pow_fast a b] calculates [a^b] efficiently for small exponents [b]. *)
  val pow_fast : integer -> int -> integer

  (* FIXME See if [pow_fast] is actually much faster under flambda. *)

  (** digits *)

  (** [fold_digits ?base n ~init ~f] folds over the digits of [n] from least significant
      to most significant. *)
  val fold_digits : ?base:integer -> integer -> init:'a -> f:('a -> integer -> 'a) -> 'a

  val iter_digits : ?base:integer -> integer -> f:(integer -> unit) -> unit
  val digits_of_int : ?base:integer -> integer -> integer list
  val int_of_digits : ?base:integer -> integer Sequence.t -> integer
  val sum_digits : ?base:integer -> integer -> integer

  (* combinatorics *)

  val factorial : integer -> integer
  val binomial : integer -> integer -> integer

  (* prime numbers *)

  val is_prime : integer -> bool
  val next_probable_prime : integer -> integer
  val next_prime : integer -> integer
  val primes : integer Sequence.t

  (* factorizations *)

  val gcd : integer -> integer -> integer
  val lcm : integer -> integer -> integer
  val factor : integer -> integer list
  val prime_factor : integer -> (integer * int) list
  val divisors : integer -> integer list
  val num_divisors : integer -> integer
  val totient : integer -> integer
  (* modular arithmetic *)

  (** [powmod a b ~modulus] returns [a^b mod modulus], without overflowing *)
  val powmod : integer -> integer -> modulus:integer -> integer

  (** [bezout a b] returns [(s, t, g)] such that [g = gcd(a, b)] and [s * a + t
   * b = g] *)
  val bezout : integer -> integer -> integer * integer * integer

  (** [chinese_remainder_theorem [(r_1, m_1); ...]] returns [(x, m)] such that
      [x % m_1 = r_1], and so on. All of [m_i] must be pairwise coprime. [m] is
      the product of [m_i]. *)
  val chinese_remainder_theorem : (integer * integer) list -> integer * integer
  (* miscellaneous sequences *)

  (** 0, 1, 1, 2, 3, 5, ... *)
  val fibonacci : integer Sequence.t

  (** calculates F(n) in O(log(n)) time, where F(0) = 0, F(1) = 1, ... *)
  val fast_fibonacci : integer -> integer

  (** [?init] defaults to 0 *)
  val natural_numbers : ?init:integer -> unit -> integer Sequence.t

  (* square roots *)

  val isqrt : integer -> integer
  val is_perfect_square : integer -> bool
end
