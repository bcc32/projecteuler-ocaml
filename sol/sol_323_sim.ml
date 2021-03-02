open! Core
open! Import

let simulate_one bits ~random_state =
  let end_ = 1 lsl bits in
  let mask = end_ - 1 in
  let x = ref 0 in
  let count = ref 0 in
  while !x <> mask do
    incr count;
    let y = Random.State.int random_state end_ in
    x := !x lor y
  done;
  !count
;;

let simulate bits times ~random_state =
  let sample_counts = Array.create 0 ~len:40 in
  for _ = 1 to times do
    let sample = simulate_one bits ~random_state in
    sample_counts.(sample) <- sample_counts.(sample) + 1
  done;
  sample_counts
;;

let expectation sample_counts total =
  let e = ref 0. in
  Array.iteri sample_counts ~f:(fun i x -> e := !e +. (float i *. float x));
  !e /. float total
;;

let main () =
  let times = 100_000_000 in
  let bits = 32 in
  let random_state = Random.State.make_self_init () in
  let samples = simulate bits times ~random_state in
  if debug then Debug.eprint_s [%sexp (samples : int array)];
  printf "%.10f\n" @@ expectation samples times
;;

(* XXX doesn't quite work *)
(* 6.3551679500
   39s *)

include
  (val Solution.make
         ~problem:
           (Tagged { number = 323; tag = "sim"; description = "Monte Carlo method" })
         ~main)
