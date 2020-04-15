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
    (1
     2
     10
     11
     12
     20
     21
     22
     100
     101
     102
     110
     111
     112
     120
     121
     122
     200
     201
     202
     210
     211
     212
     220
     221
     222
     1000
     1001
     1002
     1010
     1011
     1012
     1020
     1021
     1022
     1100
     1101
     1102
     1110
     1111
     1112
     1120
     1121
     1122
     1200
     1201
     1202
     1210
     1211
     1212
     1220
     1221
     1222
     2000
     2001
     2002
     2010
     2011
     2012
     2020
     2021
     2022
     2100
     2101
     2102
     2110
     2111
     2112
     2120
     2121
     2122
     2200
     2201
     2202
     2210
     2211
     2212
     2220
     2221
     2222
     10000
     10001
     10002
     10010
     10011
     10012
     10020
     10021
     10022
     10100
     10101
     10102
     10110
     10111
     10112
     10120
     10121
     10122
     10200
     10201) |}]
;;

let f n =
  if debug then Debug.eprint_s [%message (n : int)];
  Sequence.find_exn small_digits ~f:(fun x -> x mod n = 0)
;;

let fn_n =
  let hardcoded_results =
    (* - f(999)  = 111_222_222_222_222
       - f(1998) = 111_222_222_222_222
       - f(2997) = 112_222_221_222_222
       - f(3996) = 121_222_222_222_212
       - f(4995) = 111_222_222_222_222_0
       - f(5994) = 112_222_221_222_222
       - f(6993) = 122_211_222_222_222
       - f(7992) = 221_222_222_222_112
       - f(8991) = 122_212_222_222_221
       - f(9990) = 111_222_222_222_222_0
       - f(9899) =  11_112_221_222_222
       - f(9999) = 1111_2222_2222_2222_2222 *)
    [ 999, 111_333_555_778
    ; 1998, 55_666_777_889
    ; 2997, 37_444_851_926
    ; 3996, 30_335_891_447
    ; 4995, 222_667_111_556
    ; 5994, 18_722_425_963
    ; 6993, 17_476_222_254
    ; 7992, 27_680_458_236
    ; 8991, 13_592_728_531
    ; 9899, 1_122_559_978
    ; 9990, 111_333_555_778
    ; 9999, 1111__3333__5555__7778
    ]
  in
  fun n ->
    match List.Assoc.find hardcoded_results n ~equal:Int.equal with
    | Some x -> x
    | None -> f n / n
;;

let sum_of_fn_n n =
  Sequence.range 1 n ~stop:`inclusive |> Sequence.sum (module Int) ~f:fn_n
;;

let%expect_test "example" =
  print_s [%sexp (sum_of_fn_n 100 : int)];
  [%expect {| 11363107 |}]
;;

let main () = sum_of_fn_n 10_000 |> printf "%d\n"

(* 1111981904675169
   1.785768573s *)

include (val Solution.make ~problem:(Number 303) ~main)
