open! Core
open! Import

(* s(n) *)
let inverse_digit_sum ~digit_sum =
  let digits =
    if digit_sum % 9 = 0
    then String.make (digit_sum / 9) '9'
    else Int.to_string (digit_sum % 9) ^ String.make (digit_sum / 9) '9'
  in
  Int.of_string digits
;;

(* S(k) *)
let cumulative_inverse_digit_sum k =
  Sequence.range 1 k ~stop:`inclusive
  |> Sequence.sum (module Int) ~f:(fun n -> inverse_digit_sum ~digit_sum:n)
;;

let%expect_test "example" =
  cumulative_inverse_digit_sum 20 |> printf "%d\n";
  [%expect {| 1074 |}]
;;

let main () = raise_s [%message "unimplemented" [%here]]

include (val Solution.make ~problem:(Number 684) ~main)
