open! Core
open! Import

let%test_unit "factorial_prime_factor" =
  let gen = Gen.small_positive_int in
  Quickcheck.test gen ~sexp_of:[%sexp_of: int] ~f:(fun n ->
    let expect =
      n
      |> Bigint.of_int
      |> Number_theory.Bigint.factorial
      |> Number_theory.Bigint.prime_factor
      |> List.map ~f:(Tuple2.map_fst ~f:Bigint.to_int_exn)
    in
    [%test_result: (int * int) list] ~expect (Number_theory.factorial_prime_factor n))
;;
