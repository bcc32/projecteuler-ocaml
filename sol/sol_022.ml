open! Core
open! Import

let letter_score ch = Char.to_int ch - Char.to_int 'A' + 1
let name_score name = String.to_list name |> List.sum (module Int) ~f:letter_score

let main () =
  let names =
    Problem_022.data |> Parse.csv_line ~f:Fn.id |> List.sort ~compare:String.compare
  in
  List.mapi names ~f:(fun i name -> name_score name * (i + 1))
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

(* 5.4ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 871198282 |}]
;;

include (val Solution.make ~problem:(Number 22) ~main)
