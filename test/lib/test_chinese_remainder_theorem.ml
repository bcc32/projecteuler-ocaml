open! Core
open Bignum.Std

let%test_unit "Chinese remainder theorem" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind moduli =
      Bigint.gen_positive
      |> List.gen' ~length:(`At_least 1)
      |> Quickcheck.Generator.filter ~f:(fun ms ->
        let rec loop ls =
          match ls with
          | [] -> true
          | hd :: tl ->
            List.for_all tl ~f:(fun x ->
              Bigint.(one = Euler.Bigint.gcd x hd))
            && loop tl
        in loop ms)
    in
    let%map residues =
      List.map moduli ~f:(fun m -> Bigint.gen_incl Bigint.zero (Bigint.pred m))
      |> Quickcheck.Generator.all
    in
    List.zip_exn residues moduli
  in
  let big = Bigint.of_int in
  Quickcheck.test gen
    (* cf https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Computation *)
    ~examples:[ [ big 0, big 3
                ; big 3, big 4
                ; big 4, big 5 ] ]
    ~sexp_of:[%sexp_of: (Bigint.t * Bigint.t) list]
    ~f:(fun residues ->
      let (x, m) = Euler.Bigint.chinese_remainder_theorem residues in
      let residue_product =
        List.fold residues ~init:Bigint.one ~f:(fun ac (_, m) ->
          Bigint.(ac * m))
      in
      [%test_result: Bigint.t] m ~expect:residue_product;
      List.iter residues ~f:(fun (r, m) ->
        [%test_result: Bigint.t] Bigint.(x % m) ~expect:r))
;;
