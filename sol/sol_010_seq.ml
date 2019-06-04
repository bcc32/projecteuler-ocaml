open! Core
open! Import

let problem = Tagged { number = 10; tag = "seq"; description = "using primes Sequence.t" }

let main () =
  Number_theory.Int.primes
  |> Sequence.take_while ~f:(fun x -> x < 2_000_000)
  |> Sequence.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

include (val Solution.make ~problem ~main)
