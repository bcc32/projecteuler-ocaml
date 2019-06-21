open! Core
open! Import

let set_diff a b ~equal =
  match a, b with
  | [], _ | _, [] -> a
  | _ -> List.filter a ~f:(Fn.non (List.mem b ~equal))
;;

let iter_subsets set ~f =
  let rec loop set subset =
    match set with
    | [] -> f subset
    | hd :: tl ->
      loop tl subset;
      loop tl (hd :: subset)
  in
  loop set []
;;

let iter_allprime_subsets set ~f =
  let rec loop set min subsets =
    match set with
    | [] -> f subsets
    | set ->
      iter_subsets set ~f:(fun subset ->
        Sequences.iter_permutations subset ~compare:Int.compare ~f:(fun n ->
          let n =
            n
            |> Array.Permissioned.to_sequence_mutable
            |> Number_theory.Int.As_base10.of_sequence
          in
          if n >= min && Number_theory.Int.is_prime n
          then loop (set_diff set subset ~equal:Int.equal) (n + 1) (subset :: subsets)))
  in
  loop set 0 []
;;

let main () =
  let count = ref 0 in
  iter_allprime_subsets [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] ~f:(fun subsets ->
    if debug then Debug.eprint_s [%sexp (subsets : int list list)];
    incr count);
  printf "%d\n" !count
;;

(* 44680
   4.0339s *)

include (val Solution.make ~problem:(Number 118) ~main)
