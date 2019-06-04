open! Core
open! Import

let propagate bot top =
  let rec iter bot top acc =
    match bot, top with
    | b1 :: b2 :: bs, t :: ts ->
      let max = t + Int.max b1 b2 in
      iter (b2 :: bs) ts (max :: acc)
    | _, [] -> acc
    | _ -> Error.failwiths "length mismatched" (bot, top) [%sexp_of: int list * int list]
  in
  iter (Array.to_list bot) (Array.to_list top) [] |> Array.of_list_rev
;;

let max_sum_exn triangle =
  let triangle = Array.copy triangle in
  Array.rev_inplace triangle;
  triangle
  |> Array.reduce_exn ~f:propagate
  |> Array.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let problem = Number 18

let main () =
  let triangle = Parse.space_separated_grid Problem_018.data ~conv:Int.of_string in
  max_sum_exn triangle |> printf "%d\n"
;;

(* 0.061ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 1074 |}]
;;

include (val Solution.make ~problem ~main)
