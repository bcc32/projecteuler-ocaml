open! Core

let%test_unit "Chinese remainder theorem" =
  let gen = Quickcheck.Generator.singleton [ 0, 1 ] in
  Quickcheck.test gen
    ~examples:[ [ 0, 3
                ; 3, 4
                ; 4, 5 ] ]
    ~sexp_of:[%sexp_of: (int * int) list]
    ~f:(fun residues ->
      let (x, m) = Euler.Int.chinese_remainder_theorem residues in
      [%test_result: int] m
        ~expect:(List.fold residues ~init:1 ~f:(fun ac (_, m) -> ac * m));
      List.iter residues ~f:(fun (r, m) ->
        [%test_result: int] (x % m) ~expect:r))
