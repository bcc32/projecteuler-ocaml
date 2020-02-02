open! Core
open! Import

let birthday = 1504170715041707
let modulus = 4503599627370517 (* prime *)

let main () =
  let seq = Queue.of_list [ birthday ] in
  let x = ref birthday in
  try
    while !x <> 0 do
      let expected_guesses = 0.5 *. float modulus /. float !x in
      if Float.( < ) (float !x) expected_guesses then (* switch strategy *)
        raise Exit;
      (* advance x *)
      x := (!x + birthday) % modulus;
      if !x < Queue.last_exn seq
      then (
        if debug then Debug.eprintf "%d" !x;
        Queue.enqueue seq !x)
    done;
    assert false
  with
  | Exit ->
    let birthday', _, _ = Number_theory.Int.bezout birthday modulus in
    let birthday' = birthday' % modulus in
    let index_of =
      Array.init !x ~f:(fun k -> if k > 0 then birthday' * k % modulus else modulus)
    in
    let remaining_candidates = Array.init !x ~f:Fn.id in
    Array.sort
      remaining_candidates
      ~compare:(Comparable.lift Int.compare ~f:(fun k -> index_of.(k)));
    Array.iter remaining_candidates ~f:(fun x ->
      if x < Queue.last_exn seq
      then (
        if debug then Debug.eprintf "%d" x;
        Queue.enqueue seq x));
    if debug then Debug.eprint_s [%sexp (seq : int Queue.t)];
    printf "%d\n" (Queue.sum (module Int) seq ~f:Fn.id)
;;

(* 24.629867s *)

include (val Solution.make ~problem:(Number 700) ~main)
