open! Core
open! Import

let simulate_one bits =
  let end_ = Int.shift_left 1 bits in
  let mask = end_ - 1 in
  let x = ref 0 in
  let count = ref 0 in
  while !x <> mask do
    incr count;
    let y = Random.int end_ in
    x := Int.bit_or !x y
  done;
  !count
;;

let simulate bits times =
  let sample_counts = Array.create 0 ~len:40 in
  for _ = 1 to times do
    let sample = simulate_one bits in
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
  Random.self_init ();
  let times = 100_000_000 in
  let bits = 32 in
  let samples = simulate bits times in
  printf !"%{sexp: int array}\n" samples;
  printf "%.10f\n" @@ expectation samples times
;;

(* XXX doesn't quite work *)
(* 6.3551679500
   39s *)

include (val Solution.make
               ~problem:
                 (Tagged { number = 323; tag = "sim"; description = "Monte Carlo method" })
               ~main)
