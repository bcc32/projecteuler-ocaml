open! Core
open! Import

let partition_int : int -> int list list =
  Memo.simple
    (module Int)
    (fun n ->
       let rec partition_int n ~largest_part =
         if n = 0
         then [ [] ]
         else (
           let largest_part = Int.min n largest_part in
           List.range 1 largest_part ~stop:`inclusive
           |> List.concat_map ~f:(fun k ->
             List.map
               (partition_int (n - k) ~largest_part:k)
               ~f:(fun partition -> k :: partition)))
       in
       partition_int n ~largest_part:n)
;;

let ways_to_arrange num_digits ~nonzero_digits =
  if List.length nonzero_digits > num_digits
  then []
  else (
    let digits =
      List.init (num_digits - List.length nonzero_digits) ~f:(fun _ -> 0) @ nonzero_digits
    in
    let sequence = List.sort ~compare:Int.compare digits in
    Sequences.permutations sequence ~compare:Int.compare
    |> Sequence.map ~f:(fun permutation ->
      Number_theory.Int.As_base10.of_list permutation)
    |> Sequence.to_list)
;;

module Bigint_with_modulus = struct
  type t = Bigint.t

  let zero = Bigint.zero

  (* 10^16 *)
  let modulus = Bigint.of_int 10_000_000_000_000_000
  let ( + ) a b = Bigint.( % ) (Bigint.( + ) a b) modulus
end

(* TODO: Move this function into [Number_theory]. *)
let bigint_multinomial ints =
  let num =
    Number_theory.Bigint.factorial (Bigint.of_int (List.sum (module Int) ints ~f:Fn.id))
  in
  let den =
    List.map ints ~f:(fun n -> Number_theory.Bigint.factorial (Bigint.of_int n))
    |> List.fold ~init:Bigint.one ~f:Bigint.( * )
  in
  Bigint.( / ) num den
;;

let sum_of_ways_to_arrange num_digits ~nonzero_digits =
  if List.length nonzero_digits > num_digits
  then Bigint.zero
  else (
    let digits =
      List.init (num_digits - List.length nonzero_digits) ~f:(fun _ -> 0) @ nonzero_digits
      |> List.sort ~compare:Int.compare
      |> Sequences.run_length_encode ~equal:Int.equal
    in
    let all_digits_contribution_to_each_pos =
      let total_combinations = bigint_multinomial (List.map digits ~f:snd) in
      List.sum
        (module Bigint_with_modulus)
        digits
        ~f:(fun (digit, count) ->
          if digit = 0
          then Bigint.zero
          else
            let open Bigint.O in
            (* Basically, this is the number of possible DS-numbers times the
               probability that a given position takes the value of this
               digit. *)
            let number_of_times_this_digit_appers_in_arbitrary_position =
              total_combinations
              * Bigint.of_int count
              / Bigint.of_int (List.sum (module Int) digits ~f:snd)
            in
            Bigint.of_int digit * number_of_times_this_digit_appers_in_arbitrary_position)
    in
    List.range 0 num_digits
    |> List.sum
         (module Bigint_with_modulus)
         ~f:(fun pos ->
           if pos >= 16
           then Bigint.zero
           else
             let open Bigint.O in
             Bigint.pow (Bigint.of_int 10) (Bigint.of_int pos)
             * all_digits_contribution_to_each_pos))
;;

let digit_sum_numbers num_digits =
  List.range 1 9 ~stop:`inclusive
  |> List.concat_map ~f:(fun largest_digit ->
    partition_int largest_digit
    |> List.concat_map ~f:(fun partition ->
      ways_to_arrange num_digits ~nonzero_digits:(largest_digit :: partition)))
;;

let%expect_test "example digit sum numbers" =
  let result =
    Set.Named.is_subset
      { name = "example digit sum numbers"; set = Int.Set.of_list [ 352; 3003; 32812 ] }
      ~of_:
        { name = "generated digit sum numbers"
        ; set = Int.Set.of_list (digit_sum_numbers 5)
        }
  in
  require
    [%here]
    (Or_error.is_ok result)
    ~if_false_then_print_s:(lazy [%sexp (result : unit Or_error.t)]);
  [%expect {||}]
;;

let s_naive num_digits = digit_sum_numbers num_digits |> List.sum (module Int) ~f:Fn.id

let%expect_test "S_naive(3)" =
  print_s [%sexp (s_naive 3 : int)];
  [%expect {| 63270 |}]
;;

let%expect_test "S_naive(7)" =
  print_s [%sexp (s_naive 7 : int)];
  [%expect {| 85499991450 |}]
;;

let s num_digits =
  List.range 1 9 ~stop:`inclusive
  |> List.sum
       (module Bigint_with_modulus)
       ~f:(fun largest_digit ->
         partition_int largest_digit
         |> List.sum
              (module Bigint_with_modulus)
              ~f:(fun partition ->
                sum_of_ways_to_arrange
                  num_digits
                  ~nonzero_digits:(largest_digit :: partition)))
  |> Bigint.to_int_exn
;;

let%expect_test "S(3)" =
  print_s [%sexp (s 3 : int)];
  [%expect {| 63270 |}]
;;

let%expect_test "S(7)" =
  print_s [%sexp (s 7 : int)];
  [%expect {| 85499991450 |}]
;;

let main () = print_s [%sexp (s 2020 : int)]

(* 246.333ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 4598797036650685 |}]
;;

include (val Solution.make ~problem:(Number 725) ~main)
