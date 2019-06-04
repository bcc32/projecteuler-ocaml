open! Core
open! Import

let%test_unit "integrate" =
  let open Bignum.O in
  Quickcheck.test
    [%quickcheck.generator: Bignum.t -> Bignum.t]
    ~examples:[ (fun x -> x ** 2) ]
    ~f:(fun f ->
      let y =
        Numerics.Bignum.integrate
          ()
          ~method_:`Simpson's_rule
          ~f
          ~lo:(of_int 1)
          ~hi:(of_int 5)
          ~intervals:2
      in
      let expect =
        let interval ~f ~x_lo ~x_mi ~x_hi =
          let f = f << of_int in
          Int.O.(x_hi - x_lo) // 6 * (f x_lo + (of_int 4 * f x_mi) + f x_hi)
        in
        interval ~f ~x_lo:1 ~x_mi:2 ~x_hi:3 + interval ~f ~x_lo:3 ~x_mi:4 ~x_hi:5
      in
      [%test_result: Bignum.t] y ~expect)
;;
