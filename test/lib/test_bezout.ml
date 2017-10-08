open! Core

let%test_unit "Extended Euclidean Algorithm" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map a = Quickcheck.Generator.small_non_negative_int
    and     b = Quickcheck.Generator.small_non_negative_int in
    (a, b)
  in
  Quickcheck.iter gen ~f:(fun (a, b) ->
    let (s, t, g) = Euler.Int.bezout a b in
    [%test_result: int] g ~expect:(Euler.Int.gcd a b);
    [%test_result: int] (s * a + t * b) ~expect:g)
;;
