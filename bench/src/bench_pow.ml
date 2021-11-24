open! Core
open! Import

let%bench_module "integer exponentiation" =
  (module struct
    let exponents = [ 0; 1; 2; 4; 5; 10; 15; 20; 25; 30 ]

    let%bench ("Int.pow" [@indexed exp = exponents]) = Int.pow 3 exp

    let%bench ("pow_fast without functor" [@indexed exp = exponents]) =
      Number_theory.Int.addition_chain_pow 3 exp
    ;;
  end)
;;

let%bench_module "integer exponentiation with compile-time constant exponent" =
  (module struct
    let%bench "Int.pow" = Int.pow 3 5
    let%bench "pow_fast without functor" = Number_theory.Int.addition_chain_pow 3 5
  end)
;;
