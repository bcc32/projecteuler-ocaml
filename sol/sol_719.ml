open! Core
open! Import

(* Inspired by NabiNaga's solution at
   https://projecteuler.net/thread=719;page=3#365650 *)
let rec can_sum ~digits_of:n ~target =
  if n = target
  then true
  else if n < target
  then false
  else (
    let rec loop pow10 =
      if pow10 >= n
      then false
      else if can_sum ~digits_of:(n / pow10) ~target:(target - (n % pow10))
      then true
      else loop (pow10 * 10)
    in
    loop 10)
;;

(* As noted by ecnerwala, https://projecteuler.net/thread=719#359366,

   Splitting numbers preserves the digit sum and thus the value mod 9.
   Therefore, we only need to check the numbers whose squares are congruent to
   them mod 9, i.e., sqrt in {0, 1} mod 9. *)

let is_s_number ~sqrt ~n =
  let mod_9 = sqrt % 9 in
  (mod_9 = 0 || mod_9 = 1) && can_sum ~digits_of:n ~target:sqrt
;;

let t ~max_sqrt =
  let squares =
    Sequence.range 2 max_sqrt ~stop:`inclusive |> Sequence.map ~f:(fun x -> x, x * x)
  in
  Sequence.filter squares ~f:(fun (sqrt, n) -> is_s_number ~sqrt ~n)
;;

let%expect_test "T(10^4)" =
  let s_numbers = t ~max_sqrt:100 in
  Sequence.sum (module Int) s_numbers ~f:(fun (_, n) -> n) |> [%sexp_of: int] |> print_s;
  [%expect {| 41333 |}]
;;

(* 128088830547982
   981.608194ms *)
let main () =
  let max_sqrt = 1_000_000 in
  let s_numbers = t ~max_sqrt in
  s_numbers
  |> Sequence.sum (module Int) ~f:(fun (_, n) -> n)
  |> [%sexp_of: int]
  |> print_s
;;

include (val Solution.make ~problem:(Number 719) ~main)
