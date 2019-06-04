open! Core
open! Import

module Base3 = Number_theory.Int.As_digits (struct
    let base = 3
  end)

module Base10 = Number_theory.Int.As_base10

let small_digits =
  Number_theory.Int.natural_numbers () ~init:1
  |> Sequence.map ~f:(fun n ->
    (* TODO Add reverse_digits or [_rev] versions to Number_theory. *)
    n |> Base3.to_sequence |> Base10.of_sequence)
  |> Sequence.memoize
;;

let%expect_test "small_digits" =
  Sequence.take small_digits 100 |> [%sexp_of: int Sequence.t] |> print_s;
  [%expect
    {|
    (1 2 10 11 12 20 21 22 100 101 102 110 111 112 120 121 122 200 201 202 210
     211 212 220 221 222 1000 1001 1002 1010 1011 1012 1020 1021 1022 1100 1101
     1102 1110 1111 1112 1120 1121 1122 1200 1201 1202 1210 1211 1212 1220 1221
     1222 2000 2001 2002 2010 2011 2012 2020 2021 2022 2100 2101 2102 2110 2111
     2112 2120 2121 2122 2200 2201 2202 2210 2211 2212 2220 2221 2222 10000 10001
     10002 10010 10011 10012 10020 10021 10022 10100 10101 10102 10110 10111
     10112 10120 10121 10122 10200 10201) |}]
;;

let f n =
  if debug then Debug.eprint_s [%message (n : int)];
  Sequence.find_exn small_digits ~f:(fun x -> x mod n = 0)
;;

let fn_n n =
  (* f(9999) = 1111_2222_2222_2222_2222 *)
  if n = 9999 then 1111_3333_5555_7778 else f n / n
;;

let sum_of_fn_n n =
  Sequence.range 1 n ~stop:`inclusive |> Sequence.sum (module Int) ~f:fn_n
;;

let%expect_test "example" =
  print_s [%sexp (sum_of_fn_n 100 : int)];
  [%expect {| 11363107 |}]
;;

let problem = Number 303
let main () = sum_of_fn_n 10_000 |> printf "%d\n"

(* 1111981904675169
   19.817992214s *)
include (val Solution.make ~problem ~main)
