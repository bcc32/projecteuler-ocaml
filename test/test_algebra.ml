open! Core
open! Import

let%test_unit "quadratic_formula" =
  let gen =
    let open Gen.Let_syntax in
    let reasonable_ranges =
      let%map () = return ()
      and magnitude = Float.gen_incl (-5.) 5.
      and sign = Gen.of_list [ -1.; 0.; 1. ] in
      Float.copysign (exp magnitude) sign
    in
    let%map () = return ()
    and a = reasonable_ranges |> Gen.filter ~f:(fun x -> Float.(x <> 0.))
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
    let test_root =
      [%test_pred: float] (fun x ->
        let open Float.O in
        let y = (a * (x ** 2.)) + (b * x) + c in
        abs y < 1e-7)
    in
    match Algebra.quadratic_formula a b c with
    | `None -> [%test_result: int] 0 ~expect
    | `One x ->
      [%test_result: int] 1 ~expect;
      test_root x
    | `Two (x0, x1) ->
      [%test_result: int] 2 ~expect;
      [%test_pred: float * float] (fun (x, y) -> Float.(x < y)) (x0, x1);
      test_root x0;
      test_root x1)
;;
