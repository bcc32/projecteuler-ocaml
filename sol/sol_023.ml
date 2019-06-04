open! Core
open! Import

let problem = Number 23
let limit = 28123

let is_abundant n =
  let divisor_sum = Number_theory.Int.divisors n |> List.sum (module Int) ~f:Fn.id in
  divisor_sum > 2 * n
;;

let abundant_numbers =
  lazy
    (Sequence.range 12 limit ~stop:`inclusive
     |> Sequence.filter ~f:is_abundant
     |> Sequence.to_list)
;;

let main () =
  let can_sum = Array.create false ~len:(limit + 1) in
  let rec iter outer inner =
    match outer, inner with
    | [], [] -> ()
    | [], _ ->
      failwiths "unexpected state" (outer, inner) [%sexp_of: int list * int list]
    | _ :: tl, [] -> iter tl tl
    | x :: xs, y :: ys ->
      if x + y > limit
      then iter xs xs
      else (
        can_sum.(x + y) <- true;
        iter outer ys)
  in
  let abundant_numbers = force abundant_numbers in
  iter abundant_numbers abundant_numbers;
  let sum = ref 0 in
  for i = 0 to limit do
    if not can_sum.(i) then sum := !sum + i
  done;
  printf "%d\n" !sum
;;

(* 92ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 4179871 |}]
;;

include (val Solution.make ~problem ~main)
