open! Core
open! Import
module NT = Number_theory.Make (Int64)

let lcm_all n =
  Sequence.range 2 n ~stop:`inclusive
  |> Sequence.map ~f:Int64.of_int
  |> Sequence.fold ~init:1L ~f:NT.lcm
;;

let num_multiples_less_than n k = Int64.O.((n - 2L) / k)

(*
   P(s, N) = COUNT(1<n<N : streak(n) = s)

   streak(n) = COUNT_WHILE(k = 1.. : k | n - 1)
*)
let p (s : int) (n : int64) =
  let lower = lcm_all s in
  let upper = NT.lcm lower (Int64.of_int (s + 1)) in
  Int64.O.(num_multiples_less_than n lower - num_multiples_less_than n upper)
;;

let%test_unit "P(s, N)" =
  [%test_result: int64] ~expect:1L (p 3 14L);
  [%test_result: int64] ~expect:14286L (p 6 1_000_000L)
;;

let problem = Number 601

let main () =
  let sum = ref 0L in
  for i = 1 to 31 do
    sum := Int64.( + ) !sum (p i (Int64.( lsl ) 1L (2 * i)))
  done;
  printf "%Ld\n" !sum
;;

(* 1617243
   0.047ms *)

include (val Solution.make ~problem ~main)
