open! Core
open Bignum.Std

let%test_unit "factorial_prime_factor" =
  let gen = Quickcheck.Generator.small_positive_int in
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: int]
    ~f:(fun n ->
      let expect =
        n
        |> Bigint.of_int
        |> Euler.Bigint.factorial
        |> Euler.Bigint.prime_factor
        |> List.map ~f:(Tuple2.map_fst ~f:Bigint.to_int_exn)
      in
      [%test_result: (int * int) list] ~expect
        (Euler.factorial_prime_factor n))
;;
