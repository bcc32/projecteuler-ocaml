open! Core
open! Import

let word_value word =
  let letter_value letter = Char.to_int letter - Char.to_int 'A' + 1 in
  String.to_list word |> List.sum (module Int) ~f:letter_value
;;

let is_triangle_number =
  Memo.simple
    (module Int)
    (fun t ->
      let n = Float.(sqrt (of_int t * 2.0) |> to_int) in
      t = n * (n + 1) / 2)
;;

let main () =
  Problem_042.data
  |> Parse.csv_line ~f:Fn.id
  |> List.map ~f:word_value
  |> List.count ~f:is_triangle_number
  |> printf "%d\n"
;;

(* 0.708ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 162 |}]
;;

include (val Solution.make ~problem:(Number 42) ~main)
