open! Core
open! Import

let main () =
  let pattern = "1.2.3.4.5.6.7.8.9.0" in
  let pattern_re = Re.Perl.compile_pat pattern in
  let sqrt_replace digit ~dir =
    String.tr pattern ~target:'.' ~replacement:digit
    |> Int.of_string
    |> Float.of_int
    |> sqrt
    |> Float.iround_exn ~dir
  in
  let lb = sqrt_replace '0' ~dir:`Down |> Int.round_down ~to_multiple_of:10 in
  let ub = sqrt_replace '9' ~dir:`Up |> Int.round_up ~to_multiple_of:10 in
  Sequence.range lb ub ~stop:`inclusive ~stride:10
  |> Sequence.find_exn ~f:(fun n -> n * n |> Int.to_string |> Re.execp pattern_re)
  |> printf "%d\n"
;;

(* 1389019170
   16s *)

include (val Solution.make ~problem:(Number 206) ~main)
