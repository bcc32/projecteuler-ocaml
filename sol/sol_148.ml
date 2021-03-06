open! Core
open! Import

(* Entry (n, r) in Pascal's triangle is equal to

   C(n, r) = n! / (r! (n-r)!).

   Let S(n) be the maximum k such that 7^k divides n!.  Then, S(n) = floor(n /
   7) + floor(n / 7^2) + floor(n / 7^3) + ...

   C(n, r) is divisible by 7 when S(n) > S(r) + S(n - r).  Therefore, C(n, r) is
   not divisible by 7 when S(n) = S(r) + S(n - r), since, clearly, S(n) >= S(r)
   + S(n - r). *)

let sevens n =
  let rec loop power_of_seven acc =
    if power_of_seven > n
    then acc
    else loop (power_of_seven * 7) (acc + (n / power_of_seven))
  in
  loop 7 0
;;

let%expect_test "sevens" =
  print_s [%sexp (sevens 49 : int)];
  [%expect {| 8 |}]
;;

let entry_is_divisible_by_seven n r = sevens n - sevens r - sevens (n - r) > 0

let%expect_test "first hundred rows" =
  let count = ref 0 in
  for n = 0 to 99 do
    for r = 0 to n do
      if not (entry_is_divisible_by_seven n r) then incr count
    done
  done;
  print_s [%sexp (!count : int)];
  [%expect {| 2361 |}]
;;

(* Pattern of the sequence "number of r for which entry C(n, r) is not divisible
   by 7, for given n" seems to be:

   {v
      1 2 3 4 5 6 7
      2 4 6 8 10 12 14
      3 6 9 12 15 18 21
      4 8 12 16 20 24 28
      5 10 15 20 25 30 35
      6 12 18 24 30 36 42
      7 14 21 28 35 42 49

      2 4 6 8 10 12 14
      4 8 12 16 20 24 28
      6 12 18 24 30 36 42
      8 16 24 32 40 48 56
      10 20 30 40 50 60 70
      12 24 36 48 60 72 84
      14 28 42 56 70 84 98

      3 6 9 12 15 18 21
      6 12 18 24 30 36 42
      9 18 27 36 45 54 63
      12 24 36 48 60 72 84
      15 30 45 60 75 90 105
      18 36 54 72 90 108 126
      21 42 63 84 105 126 147

      4 8 12 16 20 24 28
      8 16 24 32 40 48 56
      12 24 36 48 60 72 84
      16 32 48 64 80 96 112
      20 40 60 80 100 120 140
      24 48 72 96 120 144 168
      28 56 84 112 140 168 196

      5 10 15 20 25 30 35
      10 20 30 40 50 60 70
      15 30 45 60 75 90 105
      20 40 60 80 100 120 140
      25 50 75 100 125 150 175
      30 60 90 120 150 180 210
      35 70 105 140 175 210 245

      6 12 18 24 30 36 42
      12 24 36 48 60 72 84
      18 36 54 72 90 108 126
      24 48 72 96 120 144 168
      30 60 90 120 150 180 210
      36 72 108 144 180 216 252
      42 84 126 168 210 252 294

      7 14 21 28 35 42 49
      14 28 42 56 70 84 98
      21 42 63 84 105 126 147
      28 56 84 112 140 168 196
      35 70 105 140 175 210 245
      42 84 126 168 210 252 294
      49 98 147 196 245 294 343

      2 4 6 8 10 12 14
      4 8 12 16 20 24 28
      6 12 18 24 30 36 42
      8 16 24 32 40 48 56
      10 20 30 40 50 60 70
      12 24 36 48 60 72 84
      14 28 42 56 70 84 98

      4 8 12 16 20 24 28
      8 16 24 32 40 48 56
      12 24 36 48 60 72 84
      16 32 48 64 80 96 112
      20 40 60 80 100 120 140
      24 48 72 96 120 144 168
      28 56 84 112 140 168 196

      6 12 18 24 30 36 42
      12 24 36 48 60 72 84
      18 36 54 72 90 108 126
      24 48 72 96 120 144 168
      30 60 90 120 150 180 210
      36 72 108 144 180 216 252
      42 84 126 168 210 252 294
   v} *)

(* This sequence can be seen as the sequence of numbers generated by taking the
   product of the digits of consecutive readings of an odometer whose digits
   range in [1, 7].  That is, [1*1*1, 1*1*2, 1*1*3, ..., 1*1*7, 1*2*1, 1*2*2,
   ...] but with an unbounded number of digits to the left. *)

let sum_rows limit =
  let depth = Float.iround_up_exn (Float.log (float limit) /. Float.log 7.) in
  let count = ref 0 in
  let digits = List.range 1 7 ~stop:`inclusive in
  let rec loop depth ~product ~sum =
    if !count >= limit
    then sum
    else if depth = 0
    then (
      incr count;
      if debug && !count % 1_000_000 = 0 then Debug.eprintf "%d" !count;
      product + sum)
    else
      List.fold digits ~init:sum ~f:(fun sum digit ->
        loop (depth - 1) ~product:(product * digit) ~sum)
  in
  loop depth ~product:1 ~sum:0
;;

let%expect_test "first hundred rows" =
  print_s [%sexp (sum_rows 100 : int)];
  [%expect {| 2361 |}]
;;

let limit = 1_000_000_000
let main () = sum_rows limit |> printf "%d\n"

(* 2129970655314432
   14.6872s *)

include (val Solution.make ~problem:(Number 148) ~main)
