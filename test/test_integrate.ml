open! Core
open! Import

let%test_unit "integrate" =
  let open Bignum.O in
  let f x = x ** 2 in
  let y =
    Numerics.Bignum.integrate ()
      ~method_:`Simpson's_rule
      ~f
      ~low:(of_int 1)
      ~high:(of_int 5)
      ~intervals:2
  in
  let expect =
    let x1 = of_int 1 in
    let x2 = of_int 2 in
    let x3 = of_int 3 in
    let x4 = of_int 4 in
    let x5 = of_int 5 in
    (x3 - x1) / of_int 6 * (f x1 + of_int 4 * f x2 + f x3)
    + (x5 - x3) / of_int 6 * (f x3 + of_int 4 * f x4 + f x5)
  in
  [%test_result: Bignum.t] y ~expect
;;
