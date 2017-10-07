open! Core

module Make (Int : Int_intf.S_unbounded) : Number_theory_intf.S
  with type integer = Int.t

module Int    : Number_theory_intf.S with type integer = int
module Bigint : Number_theory_intf.S with type integer = Bignum.Std.Bigint.t

val prime_sieve : int -> bool array
val multinomial : int list -> int
