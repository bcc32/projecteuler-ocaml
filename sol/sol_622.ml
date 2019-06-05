open! Core
open! Import

let riffle_shuffle cards =
  let half = Array.length cards / 2 in
  let left = Array.sub cards ~pos:0 ~len:half in
  let right = Array.sub cards ~pos:half ~len:half in
  for i = 0 to half - 1 do
    cards.(2 * i) <- left.(i);
    cards.((2 * i) + 1) <- right.(i)
  done
;;

let num_shuffles_naive n =
  let are_in_order = Array.for_alli ~f:Int.equal in
  let cards = Array.init n ~f:Fn.id in
  let rec loop count =
    riffle_shuffle cards;
    if are_in_order cards then count else loop (count + 1)
  in
  loop 1
;;

let%expect_test _ =
  List.range 2 256 ~stop:`inclusive
  |> List.filter ~f:(fun x -> x % 2 = 0 && num_shuffles_naive x = 8)
  |> [%sexp_of: int list]
  |> print_s;
  [%expect {|
    (18 52 86 256) |}]
;;

(* See https://en.wikipedia.org/wiki/Out_shuffle and
   http://mathworld.wolfram.com/MultiplicativeOrder.html *)

let moduli_for_which_multiplicative_order_of_2_is ~target =
  let a = (1 lsl target) - 1 in
  let divisors = Number_theory.Int.divisors a in
  List.filter divisors ~f:(fun x ->
    let rec loop a' =
      if a' = 0 then true else if a' % x = 0 then false else loop (a' / 2)
    in
    loop (a / 2))
;;

let%expect_test _ =
  moduli_for_which_multiplicative_order_of_2_is ~target:8
  |> [%sexp_of: int list]
  |> print_s;
  [%expect {|
    (17 51 85 255) |}]
;;

let target = 60

let main () =
  moduli_for_which_multiplicative_order_of_2_is ~target
  |> List.sum (module Int) ~f:succ
  |> printf "%d\n"
;;

(* 6.762ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 3010983666182123972 |}]
;;

include (val Solution.make ~problem:(Number 622) ~main)
