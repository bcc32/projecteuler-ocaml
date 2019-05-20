open! Core
open! Import

let%test_unit "digits" =
  let check_round_trip (n, digits) =
    [%test_result: int] ~expect:n (Number_theory.Int.As_base10.of_list digits);
    [%test_result: int list] ~expect:digits (Number_theory.Int.As_base10.to_list n)
  in
  let digits_through_string n =
    n |> Int.to_string |> String.to_list |> List.map ~f:Char.get_digit_exn
  in
  let gen =
    let open Quickcheck.Let_syntax in
    let%map_open n = small_positive_int in
    n, digits_through_string n
  in
  Q.test
    gen
    ~examples:
      [ 0, [ 0 ]; 1, [ 1 ]; 10, [ 1; 0 ]; 123, [ 1; 2; 3 ]; 54312, [ 5; 4; 3; 1; 2 ] ]
    ~f:check_round_trip
;;

let%expect_test "is_prime" =
  let show a b =
    Number_theory.Int.range a b
    |> Sequence.filter ~f:Number_theory.Int.is_prime
    |> [%sexp_of: int Sequence.t]
    |> print_s
  in
  show 1 10;
  [%expect {| (2 3 5 7) |}];
  show 150 160;
  [%expect {| (151 157) |}]
;;

let%test_unit "multinomial" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind length = Int.gen_incl 0 4 in
    let small_number = Int.gen_incl 0 4 in
    List.gen_with_length length small_number
  in
  Q.test gen ~sexp_of:[%sexp_of: int list] ~f:(fun xs ->
    let numerator = List.sum (module Int) xs ~f:Fn.id |> Number_theory.Int.factorial in
    let expect =
      List.fold xs ~init:numerator ~f:(fun ac x -> ac / Number_theory.Int.factorial x)
    in
    [%test_result: int] ~expect (Number_theory.multinomial xs))
;;

let%test_unit "powmod" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%map_open () = return ()
    and a = small_non_negative_int
    and b = small_non_negative_int
    and m = small_positive_int in
    a, b, m
  in
  Q.test gen ~sexp_of:[%sexp_of: int * int * int] ~f:(fun (a, b, modulus) ->
    let expect =
      let rec loop b ac = if b = 0 then ac else loop (b - 1) (ac * a % modulus) in
      loop b 1
    in
    [%test_result: int] ~expect (Number_theory.Int.powmod a b ~modulus))
;;

let%test_unit "fast_fibonacci" =
  let gen = Int.gen_incl 0 200 in
  Q.test gen ~sexp_of:[%sexp_of: int] ~f:(fun n ->
    let expect = Sequence.nth_exn Number_theory.Int.fibonacci n in
    [%test_result: int] ~expect (Number_theory.Int.fast_fibonacci n))
;;

let%expect_test "prime_factor" =
  [ 10; 17; 109; 256; 480; 9001 ]
  |> List.iter ~f:(fun x ->
    print_s [%sexp (x, Number_theory.Int.prime_factor x : int * (int * int) list)]);
  [%expect
    {|
    (10 (
      (2 1)
      (5 1)))
    (17 ((17 1)))
    (109 ((109 1)))
    (256 ((2 8)))
    (480 (
      (2 5)
      (3 1)
      (5 1)))
    (9001 ((9001 1)))
  |}]
;;

let%test_unit "isqrt_int" =
  Q.test (Int.gen_incl 0 Int.max_value) ~sexp_of:[%sexp_of: int] ~f:(fun n ->
    match Number_theory.Int.isqrt n with
    | isqrt -> [%test_result: int] isqrt ~expect:(Number_theory.Int.isqrt n)
    (* would overflow *)
    | exception _ -> ())
;;
