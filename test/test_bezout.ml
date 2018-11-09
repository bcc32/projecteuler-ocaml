open! Core
open! Import

let%test_unit "Extended Euclidean Algorithm" =
  let gen =
    let open Gen.Let_syntax in
    let%map () = return ()
    and a = Gen.small_non_negative_int
    and b = Gen.small_non_negative_int in
    a, b
  in
  Q.iter gen ~f:(fun (a, b) ->
    let s, t, g = Number_theory.Int.bezout a b in
    [%test_result: int] g ~expect:(Number_theory.Int.gcd a b);
    [%test_result: int] ((s * a) + (t * b)) ~expect:g)
;;
