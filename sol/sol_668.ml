open! Core
open! Import

let is_square_root_smooth n =
  Number_theory.Int.prime_factor n |> List.for_all ~f:(fun (p, _) -> p * p < n)
;;

let count_square_root_smooth ~max_n =
  Sequence.range 1 max_n ~stop:`inclusive |> Sequence.count ~f:is_square_root_smooth
;;

let%expect_test "example" =
  print_s [%sexp (count_square_root_smooth ~max_n:100 : int)];
  [%expect {| 29 |}]
;;

let main () = count_square_root_smooth ~max_n:10_000_000_000 |> printf "%d\n"

include (val Solution.make ~problem:(Number 668) ~main)
