open! Core

let%test_unit "multinomial" =
  let gen =
    let small_number = Int.gen_incl 0 4 in
    List.gen' small_number ~length:(`At_most 4)
  in
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun xs ->
      let numerator = List.sum (module Int) xs ~f:Fn.id |> Euler.Int.factorial in
      let expect =
        List.fold xs ~init:numerator ~f:(fun ac x ->
          ac / Euler.Int.factorial x)
      in
      [%test_result: int] ~expect
        (Euler.multinomial xs))
;;

let%test_unit "powmod" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map a = Quickcheck.Generator.small_non_negative_int
    and     b = Quickcheck.Generator.small_non_negative_int
    and     m = Quickcheck.Generator.small_positive_int in
    (a, b, m)
  in
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: int * int * int]
    ~f:(fun (a, b, modulus) ->
      let expect =
        let rec loop b ac =
          if b = 0
          then ac
          else (loop (b - 1) ((ac * a) % modulus))
        in
        loop b 1
      in
      [%test_result: int] ~expect (Euler.Int.powmod a b ~modulus))
;;
