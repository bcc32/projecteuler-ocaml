open! Core
open! Import

(** {%latex \( \sum_{j = 1}^{N} \sum_{i = 1}^{j} \gcd(i, j) \) %} *)

(* let sum_gcd_upto n =
 *   Number_theory.Int.prime_factor n
 *   |> List.sum (module Int) ~f:(fun (p, a) ->
 *   )
 * ;; *)

let sum_gcd_upto n =
  Sequence.range 1 n ~stop:`inclusive
  |> Sequence.sum
       (module Int)
       ~f:(fun j ->
         Sequence.range 1 j ~stop:`inclusive
         |> Sequence.sum (module Int) ~f:(fun i -> Number_theory.Int.gcd i j))
;;

let%expect_test "sum_gcd_upto" =
  let gcds = List.range 1 10 ~stop:`inclusive |> List.map ~f:sum_gcd_upto in
  print_s [%sexp (gcds : int list)];
  [%expect {| (1 4 9 17 26 41 54 74 95 122) |}]
;;

module M = struct
  let problem = `Number 625
  let main () = sum_gcd_upto 10 |> printf "%d\n"
end

include Solution.Make (M)
