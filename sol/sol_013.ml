open! Core
open! Import

let problem = Number 13
let prefix_length = 10

let main () =
  Problem_013.data
  |> String.split_lines
  |> List.sum (module Bigint) ~f:Bigint.of_string
  |> Bigint.to_string
  |> String.subo ~len:prefix_length
  |> printf "%s\n"
;;

(* 0.11ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 5537376230 |}]
;;

include (val Solution.make ~problem ~main)
