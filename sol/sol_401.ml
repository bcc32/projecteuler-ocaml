open! Core
open! Import

let run_all_unit_tests = false

(* TODO: Add this to euler_lib. *)
(*
   {[
     module Int_detecting_overflow = struct
       let ( + ) a b =
         if b > 0 && a > Int.max_value - b
         then raise_s [%message "addition would overflow" (a : int) (b : int)];
         if b < 0 && a < Int.min_value - b
         then raise_s [%message "addition would underflow" (a : int) (b : int)];
         a + b
       ;;

       let ( - ) a b =
         if b < 0 && a > Int.max_value + b
         then raise_s [%message "subtraction would overflow" (a : int) (b : int)];
         if b > 0 && a < Int.min_value + b
         then raise_s [%message "subtraction would underflow" (a : int) (b : int)];
         a - b
       ;;

       let ( * ) a b =
         if a = -1 && b = Int.min_value
         then raise_s [%message "multiplication would overflow" (a : int) (b : int)];
         if b = -1 && a = Int.min_value
         then raise_s [%message "multiplication would overflow" (a : int) (b : int)];
         if b <> 0 && a > Int.max_value / b
         then raise_s [%message "multiplication would overflow" (a : int) (b : int)];
         if b <> 0 && a < Int.min_value / b
         then raise_s [%message "multiplication would underflow" (a : int) (b : int)];
         a * b
       ;;
     end

     open Int_detecting_overflow

     module Strict_integer_division = struct
       let ( / ) a b =
         if a % b <> 0
         then raise_s [%message "division would truncate" (a : int) (b : int)];
         a / b
       ;;
     end
   ]}
*)

module Bigint_with_modulus = struct
  include Bigint

  let modulus = of_int 1_000_000_000
  let ( + ) a b = (a + b) % modulus
  let ( - ) a b = (a - b) % modulus
  let ( * ) a b = a * b % modulus
end

let sIGMA2_naive n =
  let open Bigint_with_modulus in
  let sum = ref zero in
  for i = 1 to n do
    let num_multiples = of_int n / of_int i in
    sum := !sum + (of_int i * of_int i * num_multiples)
  done;
  !sum
;;

let%expect_test "small n examples" =
  for n = 1 to 6 do
    print_s [%sexp (sIGMA2_naive n : Bigint.t)]
  done;
  [%expect {|
    1
    6
    16
    37
    63
    113 |}]
;;

(* SIGMA2(n) is equivalent to the sum of d^2 * k for all pairs (d, k) where [1
   <= d <= n] and [k] is the largest integer such that [dk <= n].

   This can be easily seen be re-ordering the terms of the summation. *)

(** [divisor_frontier n] is a set of pairs (a, b), where WLOG [a <= b], and [ab
    <= n], but [(a+1)b > n] and [a(b+1) > n].  [a] ranges from [1] to
    [isqrt(n)]. *)
let divisor_frontiers n =
  Sequence.range 1 (Number_theory.Int.isqrt n) ~stop:`inclusive
  |> Sequence.map ~f:(fun a -> a, n / a)
;;

let%expect_test "divisor_frontiers" =
  divisor_frontiers 10 |> [%sexp_of: (int * int) Sequence.t] |> print_s;
  [%expect {|
    ((1 10)
     (2 5)
     (3 3)) |}]
;;

(** [divisor_frontier_ranges n] returns a sequence of triples [(k, d1, d2)]
    where [k] is the maximum number of multiples of [d1 <= d <= d2] that can fit
    in [n]. *)
let divisor_frontier_ranges n =
  let divisor_frontiers = divisor_frontiers n |> Sequence.to_list_rev in
  Sequence.Generator.run
    (let open Sequence.Generator.Let_syntax in
     let rec loop divisor_frontiers last_d2 =
       match divisor_frontiers with
       | [] -> return ()
       | (k, d2) :: tl ->
         let d1 = last_d2 + 1 in
         let%bind () = Sequence.Generator.yield (k, d1, d2) in
         loop tl d2
     in
     let k, d = List.hd_exn divisor_frontiers in
     (* There might be a gap between isqrt(n) and n / isqrt(n), so we fill it
        in. *)
     let%bind () = Sequence.Generator.yield (k, Int.min d (k + 1), d) in
     loop (List.tl_exn divisor_frontiers) d)
;;

let%expect_test "divisor_frontier_ranges" =
  divisor_frontier_ranges 10 |> [%sexp_of: (int * int * int) Sequence.t] |> print_s;
  [%expect {|
    ((3 3 3)
     (2 4 5)
     (1 6 10)) |}]
;;

let%test_unit "divisor_frontier_ranges covers all numbers in [1,n]" =
  Quickcheck.test
    Quickcheck.Generator.small_positive_int
    ~sexp_of:[%sexp_of: int]
    ~f:(fun n ->
      let ints_contained =
        divisor_frontier_ranges n
        |> Sequence.concat_map ~f:(fun (k, d1, d2) ->
          Sequence.shift_right (Sequence.range d1 d2 ~stop:`inclusive) k)
        |> Sequence.fold ~init:(Set.empty (module Int)) ~f:Set.add
      in
      [%test_result: Set.M(Int).t]
        ~expect:(Set.of_list (module Int) (List.range 1 n ~stop:`inclusive))
        ints_contained)
;;

(** lo^2 + (lo+1)^2 + ... + hi^2 *)
let sum_of_range_of_squares ~lo ~hi =
  let sum_of_n_squares n =
    (* NOTE: We don't open Bigint_with_modulus here because that could break the
       clean integer division in the formula. *)
    let open Bigint in
    let n = of_int n in
    n * (n + of_int 1) * ((of_int 2 * n) + of_int 1) / of_int 6
  in
  Bigint_with_modulus.( - ) (sum_of_n_squares hi) (sum_of_n_squares (lo - 1))
;;

let%test_unit "sum_of_range_of_squares" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind lo = Int.gen_incl 1 1_000_000_000_000 in
    let%map hi = Int.gen_incl lo (lo + 1_000) in
    lo, hi
  in
  if run_all_unit_tests
  then
    Quickcheck.test
      gen
      ~sexp_of:[%sexp_of: int * int]
      ~shrinker:[%quickcheck.shrinker: int * int]
      ~examples:
        [ 1, 100_000_000
        ; 100_000_000, 100_000_000
        ; 999_999_999, 1_000_000_000
        ; 100_000_000_000, 100_000_000_000
        ; 673_883_346_566, 673_883_346_566
        ; 673_883_346_566, 673_883_347_202
        ]
      ~trials:100
      ~f:(fun (lo, hi) ->
        [%test_result: Bigint.t]
          ~expect:
            (Sequence.range lo hi ~stop:`inclusive
             |> Sequence.sum
                  (module Bigint_with_modulus)
                  ~f:(fun x ->
                    let open Bigint_with_modulus in
                    let x = of_int x % modulus in
                    x * x))
          (sum_of_range_of_squares ~lo ~hi))
;;

let sIGMA2_by_ranges n =
  divisor_frontier_ranges n
  |> Sequence.sum
       (module Bigint_with_modulus)
       ~f:(fun (k, d1, d2) ->
         let bi = Bigint.of_int in
         Bigint.( + )
           (if k < d1 then Bigint_with_modulus.O.(bi d2 * bi k * bi k) else Bigint.zero)
           Bigint.O.(bi k * sum_of_range_of_squares ~lo:d1 ~hi:d2))
;;

let%test_unit "coherence" =
  if run_all_unit_tests
  then
    Quickcheck.test
      (Int.gen_incl 1 1_000_000)
      ~sexp_of:[%sexp_of: int]
      ~examples:[ 1; 10; 100; 1_000; 10_000; 100_000 ]
      ~trials:100
      ~f:(fun n -> [%test_result: Bigint.t] ~expect:(sIGMA2_naive n) (sIGMA2_by_ranges n))
;;

let%expect_test "small n SIGMA2 by ranges" =
  for n = 1 to 6 do
    print_s [%sexp (sIGMA2_by_ranges n : Bigint.t)]
  done;
  [%expect {|
    1
    6
    16
    37
    63
    113 |}]
;;

let limit = 1_000_000_000_000_000

let main () =
  Bigint_with_modulus.(sIGMA2_by_ranges limit % modulus) |> printf !"%{Bigint}\n"
;;

(* 281632621
   19.17s *)

include (val Solution.make ~problem:(Number 401) ~main)
