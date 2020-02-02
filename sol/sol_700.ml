open! Core
open! Import

let birthday = 1504170715041707
let modulus = 4503599627370517

let main () =
  let seq = Queue.of_list [ birthday ] in
  let smallest_so_far = ref birthday in
  let should_guess = ref true in
  while !should_guess do
    let expected_guesses = 0.5 *. float modulus /. float !smallest_so_far in
    if Float.( < ) (float !smallest_so_far) expected_guesses
    then (* switch strategy *)
      should_guess := false
    else (
      (* advance [smallest_so_far] *)
      smallest_so_far := (!smallest_so_far + birthday) % modulus;
      if !smallest_so_far < Queue.last_exn seq
      then (
        if debug then Debug.eprintf "%d" !smallest_so_far;
        Queue.enqueue seq !smallest_so_far))
  done;
  let modinv = Number_theory.Int.invmod birthday ~modulus in
  let index_of =
    Array.init !smallest_so_far ~f:(fun k ->
      if k > 0 then modinv * k % modulus else modulus)
  in
  let remaining_candidates = Array.init !smallest_so_far ~f:Fn.id in
  Array.sort
    remaining_candidates
    ~compare:(Comparable.lift Int.compare ~f:(fun k -> index_of.(k)));
  Array.iter remaining_candidates ~f:(fun smallest_so_far ->
    if smallest_so_far < Queue.last_exn seq
    then (
      if debug then Debug.eprintf "%d" smallest_so_far;
      Queue.enqueue seq smallest_so_far));
  if debug then Debug.eprint_s [%sexp (seq : int Queue.t)];
  printf "%d\n" (Queue.sum (module Int) seq ~f:Fn.id)
;;

(* 24.629867s
   1517926517777556 *)

include (val Solution.make ~problem:(Number 700) ~main)
