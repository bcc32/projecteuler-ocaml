open! Core
open! Import

let%test_unit "quadratic_formula" =
  let gen =
    let open Quickcheck.Let_syntax in
    let reasonable_ranges =
      let%map_open () = return ()
      and magnitude = Float.gen_incl (-5.) 5.
      and sign = of_list [ -1.; 0.; 1. ] in
      Float.copysign (exp magnitude) sign
    in
    let%map_open () = return ()
    and a = reasonable_ranges |> filter ~f:(fun x -> Float.(x <> 0.))
    and b = reasonable_ranges
    and c = reasonable_ranges in
    a, b, c
  in
  Q.test gen ~sexp_of:[%sexp_of: float * float * float] ~f:(fun (a, b, c) ->
    let expect =
      match Float.(sign_or_nan ((b * b) - (4. * a * c))) with
      | Neg | Nan -> 0
      | Zero -> 1
      | Pos -> 2
    in
    let test_root here x =
      let open Float.O in
      let y = (a * (x ** 2.)) + (b * x) + c in
      if abs y >= 1e-7
      then
        raise_s
          [%message
            "Expected y=f(x) to be approximately zero"
              (x : float)
              (y : float)
              (here : Source_code_position.t)]
    in
    match Algebra.quadratic_formula a b c with
    | `None -> [%test_result: int] 0 ~expect
    | `One x ->
      [%test_result: int] 1 ~expect;
      test_root [%here] x
    | `Two (x0, x1) ->
      [%test_result: int] 2 ~expect;
      if Float.O.(x0 >= x1)
      then raise_s [%message "Expected root x0 < x1" (x0 : float) (x1 : float)];
      test_root [%here] x0;
      test_root [%here] x1)
;;
