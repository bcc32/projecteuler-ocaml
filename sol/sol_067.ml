open! Core
open! Import

let main () =
  Problem_067.data
  |> Parse.space_separated_grid ~conv:Int.of_string
  |> Sol_018.max_sum_exn
  |> printf "%d\n"
;;

(* 7273
   0.903ms *)
include (val Solution.make ~problem:(Number 67) ~main)
