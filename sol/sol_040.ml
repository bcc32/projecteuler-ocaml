open! Core
open! Import

let champernowne's_constant =
  let open Sequence.Let_syntax in
  let%bind nat = Number_theory.Int.natural_numbers () ~init:1 in
  Number_theory.Int.As_base10.to_sequence nat
;;

let limit = 1_000_000
let indices = [ 1; 10; 100; 1_000; 10_000; 100_000; 1_000_000 ]

let main () =
  champernowne's_constant
  |> Fn.flip Sequence.take limit
  |> Sequence.foldi ~init:1 ~f:(fun i ac digit ->
    if List.mem indices (i + 1) ~equal:Int.equal then ac * digit else ac)
  |> printf "%d\n"
;;

(* 97.183ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 210 |}]
;;

include (val Solution.make ~problem:(Number 40) ~main)
