open! Core
open! Import

let problem = Number 8

let doit str =
  Sequence.range 0 (String.length str - 12)
  |> Sequence.map ~f:(fun i ->
    String.sub str ~pos:i ~len:13
    |> Sequences.digits_of_string
    |> List.fold ~init:1 ~f:( * ))
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let main () = doit Problem_008.data |> printf "%d\n"

(* 3.3ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 23514624000 |}]
;;

include (val Solution.make ~problem ~main)
