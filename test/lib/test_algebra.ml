open! Core
open! Import

let%test_unit "quadratic_formula" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let reasonable_ranges =
      let%map magnitude = Float.gen_incl (-5.) 5. in
      exp magnitude
    in
    let%map a =
      reasonable_ranges
      |> Quickcheck.Generator.filter ~f:(fun x -> Float.(x <> 0.))
    and b = reasonable_ranges
    and c = reasonable_ranges in
    (a, b, c)
  in
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: float * float * float]
    ~f:(fun (a, b, c) ->
      let expect =
        match Float.(sign_or_nan (b * b - 4. * a * c)) with
        | Neg | Nan -> 0
        | Zero -> 1
        | Pos -> 2
      in
      match Algebra.quadratic_formula a b c with
      | `None -> [%test_result: int] 0 ~expect
      | `One x ->
        [%test_result: int] 1 ~expect;
        let y = Float.(a * x ** 2. + b * x + c) in
        assert (y < 1e-8)
      | `Two (x0, x1) ->
        [%test_result: int] 2 ~expect;
        assert (x0 < x1);
        let y0 = Float.(a * x0 ** 2. + b * x0 + c) in
        let y1 = Float.(a * x1 ** 2. + b * x1 + c) in
        assert (y0 < 1e-5);
        assert (y1 < 1e-5))
