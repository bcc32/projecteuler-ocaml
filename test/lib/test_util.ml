open! Core
open! Import

let%test_unit "digits_of_string" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 12 in
    String.gen_with_length length Char.gen_digit
  in
  Quickcheck.test gen ~f:(fun s ->
    let expect = Int.of_string s in
    [%test_result: int] ~expect (
      s
      |> Util.digits_of_string
      |> Sequence.of_list
      |> Number_theory.Int.int_of_digits))
;;

let%test_unit "is_palindrome" =
  let gen = List.gen Int.gen in
  Quickcheck.test_can_generate gen ~f:(fun l -> List.rev l = l);
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun l ->
      let expect = List.rev l = l in
      [%test_result: bool] (Util.is_palindrome l ~equal:Int.equal) ~expect)
;;

let%test_unit "permutations" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 0 5 in
    List.gen_with_length length Int.gen
  in
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun l ->
      let cmp = Int.compare in
      let seq = Util.permutations l ~cmp in
      let perms = Sequence.to_list seq in
      let elts = List.sort l ~cmp in
      (* expected number of permutations *)
      let num_permutations =
        List.fold l ~init:Int.Map.empty ~f:(fun ac x ->
          Map.update ac x ~f:(Option.value_map ~default:1 ~f:((+) 1)))
        |> Map.data
        |> Number_theory.multinomial
      in
      [%test_result: int] (List.length perms) ~expect:num_permutations;
      (* permutations have the same elements as the original list *)
      List.iter perms ~f:(fun perm ->
        [%test_result: int list] (List.sort perm ~cmp) ~expect:elts);
      (* permutations should be in order *)
      [%test_result: int list list] ~expect:perms
        (List.sort perms ~cmp:[%compare: int list]);
      (* permutations should be unique *)
      [%test_result: int list option] ~expect:None
        (List.find_a_dup perms ~compare:[%compare: int list]))
;;

let%test_unit "run_length_encode" =
  let gen = List.gen Int.gen in
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun l ->
      let enc = Util.run_length_encode l ~equal:Int.equal in
      (* reconstruct the list *)
      [%test_result: int list] ~expect:l
        (List.bind enc ~f:(fun (elt, n) ->
           assert (n > 0);
           List.init n ~f:(fun _ -> elt)));
      (* no consecutive duplicates *)
      [%test_result: ((int * int) * (int * int)) option] ~expect:None
        (List.find_consecutive_duplicate enc
           ~equal:(fun (e1, _) (e2, _) -> e1 = e2)))
;;
