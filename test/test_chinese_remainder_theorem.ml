open! Core
open! Import

let are_pairwise_coprime xs =
  let rec loop xs =
    match xs with
    | [] -> true
    | hd :: tl ->
      List.for_all tl ~f:(fun x -> Bigint.(one = Number_theory.Bigint.gcd x hd))
      && loop tl
  in
  loop xs
;;

let%test_unit "Chinese remainder theorem" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind_open moduli =
      Bigint.gen_positive |> list_non_empty |> filter ~f:are_pairwise_coprime
    in
    List.map moduli ~f:(fun modulus ->
      let%map residue = Bigint.gen_incl Bigint.zero (Bigint.pred modulus) in
      residue, modulus)
    |> Quickcheck.Generator.all
  in
  Q.test
    gen
    (* example from https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Computation *)
    ~examples:
      [ [ 0, 3; 3, 4; 4, 5 ]
        |> List.map ~f:(fun (x, y) -> Bigint.of_int x, Bigint.of_int y)
      ]
    ~sexp_of:[%sexp_of: (Bigint.t * Bigint.t) list]
    ~f:(fun residues ->
      let x, m = Number_theory.Bigint.chinese_remainder_theorem residues in
      let residue_product =
        List.fold residues ~init:Bigint.one ~f:(fun ac (_, m) -> Bigint.(ac * m))
      in
      [%test_result: Bigint.t] m ~expect:residue_product;
      List.iter residues ~f:(fun (r, m) ->
        [%test_result: Bigint.t] Bigint.(x % m) ~expect:r))
;;
