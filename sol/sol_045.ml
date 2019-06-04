open! Core
open! Import

let problem = Number 45
let prev = 40755

let merge_keeping_only_dups xs ys =
  Sequence.merge_with_duplicates xs ys ~compare:Int.compare
  |> Sequence.filter_map ~f:(function
    | Left _ | Right _ -> None
    | Both (x, _) -> Some x)
;;

let main () =
  let map_nats ~f = Number_theory.Int.natural_numbers () ~init:1 |> Sequence.map ~f in
  let triangle = map_nats ~f:(fun n -> n * (n + 1) / 2) in
  let pentagonal = map_nats ~f:(fun n -> n * ((3 * n) - 1) / 2) in
  let hexagonal = map_nats ~f:(fun n -> n * ((2 * n) - 1)) in
  [ triangle; pentagonal; hexagonal ]
  |> List.reduce_balanced_exn ~f:merge_keeping_only_dups
  |> Sequence.drop_while ~f:(fun x -> x <= prev)
  |> Sequence.hd_exn
  |> printf "%d\n"
;;

(* 8.8ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 1533776805 |}]
;;

include (val Solution.make ~problem ~main)
