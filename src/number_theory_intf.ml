open! Core

module type S = sig
  type integer

  val range
    :  ?stride:integer
    -> ?start:[ `inclusive | `exclusive ]
    -> ?stop:[ `exclusive | `inclusive ]
    -> integer -> integer -> integer Sequence.t

  (* digits *)
  val digits_of_int : ?base:integer -> integer -> integer list
  val int_of_digits : ?base:integer -> integer Sequence.t -> integer
  val sum_digits : ?base:integer -> integer -> integer

  (* combinatorics *)
  val factorial : integer -> integer
  val binomial  : integer -> integer -> integer

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

  (* miscellaneous sequences *)
  val fibonacci : integer Sequence.t
  val natural_numbers : ?init:integer -> unit -> integer Sequence.t

  (* square roots *)
  val isqrt : integer -> integer
  val is_perfect_square : integer -> bool
end
