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
