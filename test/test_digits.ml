open! Core
open! Import

let%test_unit "basic round tripping" =
  let check_round_trip (n, digits) =
    [%test_result: int] ~expect:n (Number_theory.Int.As_base10.of_list digits);
    [%test_result: int list] ~expect:digits (Number_theory.Int.As_base10.to_list n)
  in
  let digits_through_string n =
    n |> Int.to_string |> String.to_list |> List.map ~f:Char.get_digit_exn
  in
  let gen =
    let open Quickcheck.Let_syntax in
    let%map_open n = small_non_negative_int in
    n, digits_through_string n
  in
  Q.test
    gen
    ~examples:
      [ 0, [ 0 ]; 1, [ 1 ]; 10, [ 1; 0 ]; 123, [ 1; 2; 3 ]; 54312, [ 5; 4; 3; 1; 2 ] ]
    ~f:check_round_trip
;;

module type Test_arg = sig
  type t = int * int list [@@deriving sexp_of]

  (** [quickcheck_generator] should generate pairs of [(int, digits)] where
      [digits] corresponds to the digits of [int] in the order specified by
      [M]. *)
  val quickcheck_generator : t Quickcheck.Generator.t
end

module Test_container
    (M : Number_theory.As_digits_one_direction with type integer := int)
    (T : Test_arg) : Number_theory.As_digits_one_direction with type integer := int
(* This signature helps ensure that we test all of the Container interface. *) = struct
  open M

  let base = base
  let fold = fold

  let%test_unit "fold" =
    Quickcheck.test
      [%quickcheck.generator: T.t]
      ~sexp_of:[%sexp_of: T.t]
      ~f:(fun (int, digits) ->
        let init = [] in
        let f acc d = d :: acc in
        [%test_result: int list] ~expect:(List.fold digits ~init ~f) (fold int ~init ~f))
  ;;

  let iter = iter

  let%test_unit "iter" =
    Quickcheck.test
      [%quickcheck.generator: T.t]
      ~sexp_of:[%sexp_of: T.t]
      ~f:(fun (int, digits) ->
        let list =
          let result = ref [] in
          List.iter digits ~f:(fun d -> result := d :: !result);
          !result
        in
        let as_digits =
          let result = ref [] in
          iter int ~f:(fun d -> result := d :: !result);
          !result
        in
        [%test_result: int list] ~expect:list as_digits)
  ;;

  let length = length

  let%test_unit "length" =
    Quickcheck.test
      [%quickcheck.generator: T.t]
      ~sexp_of:[%sexp_of: T.t]
      ~f:(fun (int, digits) ->
        [%test_result: int] ~expect:(List.length digits) (length int))
  ;;

  let to_list = to_list
  let to_sequence = to_sequence
  let to_array = to_array

  let%test_unit "to_list/to_sequence/to_array" =
    Quickcheck.test
      [%quickcheck.generator: T.t]
      ~sexp_of:[%sexp_of: T.t]
      ~f:(fun (int, digits) ->
        [%test_result: int list] ~expect:digits (to_list int);
        [%test_result: int Sequence.t] ~expect:(Sequence.of_list digits) (to_sequence int);
        [%test_result: int array] ~expect:(Array.of_list digits) (to_array int))
  ;;

  let of_list = of_list
  let of_sequence = of_sequence
  let of_array = of_array

  let%test_unit "of_list/of_sequence/of_array" =
    Quickcheck.test
      [%quickcheck.generator: T.t]
      ~sexp_of:[%sexp_of: T.t]
      ~f:(fun (int, digits) ->
        [%test_result: int] ~expect:int (of_list digits);
        [%test_result: int] ~expect:int (of_sequence (Sequence.of_list digits));
        [%test_result: int] ~expect:int (of_array (Array.of_list digits)))
  ;;

  let append = append

  let%test_unit "append" =
    Quickcheck.test
      [%quickcheck.generator: T.t * T.t]
      ~sexp_of:[%sexp_of: T.t * T.t]
      ~f:(fun ((int1, digits1), (int2, digits2)) ->
        [%test_result: int] ~expect:(of_list (digits1 @ digits2)) (append int1 int2))
  ;;

  (* Don't bother testing functions that are produced by the functor.  They are
     probably correct. *)

  let mem = mem
  let is_empty = is_empty
  let fold_result = fold_result
  let fold_until = fold_until
  let exists = exists
  let for_all = for_all
  let count = count
  let sum = sum
  let find = find
  let find_map = find_map
  let to_array = to_array
  let min_elt = min_elt
  let max_elt = max_elt
end

module Test_left_to_right = struct
  type t = int * int list [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Let_syntax in
    let%map_open n = small_non_negative_int in
    let digits = Int.to_string n |> String.to_list |> List.map ~f:Char.get_digit_exn in
    n, digits
  ;;
end

module Test_right_to_left = struct
  include Test_left_to_right

  let quickcheck_generator =
    let open Quickcheck.Let_syntax in
    let%map n, digits = quickcheck_generator in
    n, List.rev digits
  ;;
end

let%test_module "left-to-right" =
  (module Test_container (Number_theory.Int.As_base10.Left_to_right) (Test_left_to_right))
;;

let%test_module "right-to-left" =
  (module Test_container (Number_theory.Int.As_base10.Right_to_left) (Test_right_to_left))
;;

let%test_module "non-directional" =
  (module struct
    (** Testing functions that aren't part of the As_digits_one_direction
        signature. *)

    let%test_unit "rev" =
      Quickcheck.test
        Quickcheck.Generator.small_non_negative_int
        ~sexp_of:[%sexp_of: int]
        ~examples:[ 0; 1; 10; 12; 123 ]
        ~f:(fun n ->
          [%test_result: int]
            ~expect:(n |> Int.to_string |> String.rev |> Int.of_string)
            (Number_theory.Int.As_base10.rev n))
    ;;
  end)
;;
