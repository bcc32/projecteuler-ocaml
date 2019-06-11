open! Core
open! Import

let main () =
  Problem_105.data
  |> String.split_lines
  |> List.map ~f:(String.split ~on:',' >> Array.of_list_map ~f:Int.of_string)
  |> List.filter ~f:Sol_103.is_special
  |> List.sum (module Int) ~f:(Array.sum (module Int) ~f:Fn.id)
  |> printf "%d\n"
;;

(* 73702
   5.02189s *)
include (val Solution.make ~problem:(Number 105) ~main)
