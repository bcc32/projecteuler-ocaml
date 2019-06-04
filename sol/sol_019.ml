open! Core
open! Import

let problem = Number 19

let main () =
  let start = Date.of_string "1901-01-01" in
  let stop = Date.of_string "2000-12-31" in
  Sequence.unfold_step ~init:start ~f:(fun d ->
    if Date.O.(d > stop) then Done else Yield (d, Date.add_months d 1))
  |> Sequence.count ~f:(fun d -> [%equal: Day_of_week.t] Sun (Date.day_of_week d))
  |> printf "%d\n"
;;

(* 167.972us *)
let%expect_test "answer" =
  main ();
  [%expect {| 171 |}]
;;

include (val Solution.make ~problem ~main)
