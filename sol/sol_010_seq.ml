open! Core
open! Import

let main () =
  Number_theory.Int.primes
  |> Sequence.take_while ~f:(fun x -> x < 2_000_000)
  |> Sequence.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

(* 142913828922
   912.866719ms *)

include (val Solution.make
               ~problem:
                 (Tagged
                    { number = 10; tag = "seq"; description = "using primes Sequence.t" })
               ~main)
