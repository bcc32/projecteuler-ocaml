open! Core
open! Import

let%bench_fun "Euler.run_length_encode" =
  let length = 200 in
  let list =
    List.gen_with_length length [%quickcheck.generator: bool] |> Quickcheck.random_value
  in
  fun () -> Sequences.run_length_encode list ~equal:Bool.equal
;;
