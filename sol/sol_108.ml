open! Core
open! Import

(* TODO: After solving 110, see if the below can be made faster. *)

let num_solutions n =
  if debug && n % 1000 = 0 then Debug.eprintf "%d" n;
  let count = ref 2 in
  for i = n + 2 to (2 * n) - 1 do
    if n * i mod (i - n) = 0 then incr count
  done;
  !count
;;

let main () =
  Number_theory.Int.natural_numbers () ~init:1
  |> Sequence.find_exn ~f:(fun n -> num_solutions n > 1000)
  |> printf "%d\n"
;;

(* 180180
   3.5m *)
include (val Solution.make ~problem:(Number 108) ~main)
