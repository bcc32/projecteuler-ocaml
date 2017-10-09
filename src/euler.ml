open! Core

module Solution      = Solution
module Solution_intf = Solution_intf

module Int    = Number_theory.Int
module Bigint = Number_theory.Bigint

module Float  = Numerics.Float
module Bignum = Numerics.Bignum

let quadratic_formula = Algebra.quadratic_formula

let is_pythagorean_triple = Geometry.is_pythagorean_triple

let factorial_prime_factor = Number_theory.factorial_prime_factor
let multinomial            = Number_theory.multinomial
let prime_sieve            = Number_theory.prime_sieve

let is_palindrome     = Util.is_palindrome
let permutations      = Util.permutations
let run_length_encode = Util.run_length_encode
let digits_of_string  = Util.digits_of_string
