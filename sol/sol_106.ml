open! Core
open! Import

let number_of_subset_pair_unions n k = Number_theory.Int.binomial n (2 * k)

exception Went_negative

let _ = Went_negative

let number_of_arrangements_of_2k_elements_to_test k =
  let seq = List.init k ~f:(fun _ -> `Left) @ List.init k ~f:(fun _ -> `Right) in
  Sequences.permutations
    seq
    ~compare:
      (Comparable.lift Int.compare ~f:(function
         | `Left -> 0
         | `Right -> 1))
  |> Sequence.take_while ~f:(fun perm -> Poly.( = ) (List.hd_exn perm) `Left)
  |> Sequence.filter ~f:(fun perm ->
    Poly.( = )
      `Need_to_test
      (try
         if debug
         then Debug.eprint_s [%message "testing" (perm : [`Left | `Right] list)];
         let rec loop perm left_minus_right_in_prefix =
           match perm with
           | [] -> `Obvious
           | `Left :: tl -> loop tl (left_minus_right_in_prefix + 1)
           | `Right :: tl ->
             let left_minus_right_in_prefix = left_minus_right_in_prefix - 1 in
             if left_minus_right_in_prefix < 0 then raise Went_negative;
             loop tl left_minus_right_in_prefix
         in
         loop perm 0
       with
       | Went_negative -> `Need_to_test))
  |> Sequence.to_list
  |> fun x ->
  if debug then Debug.eprint_s [%message "permutations" (x : [`Left | `Right] list list)];
  x |> List.length
;;

(* We only need to test disjoint subsets of the same size, since rule 2 takes
   care of subsets of different sizes. *)
let number_of_subset_pairs_to_be_tested n =
  List.range 1 (n / 2) ~stop:`inclusive
  |> List.sum
       (module Int)
       ~f:(fun k ->
         let a = number_of_subset_pair_unions n k in
         let b = number_of_arrangements_of_2k_elements_to_test k in
         if debug
         then Debug.eprint_s [%message "" (k : int) (a : int) (b : int) (a * b : int)];
         a * b)
;;

module M = struct
  let problem = Number 106
  let main () = number_of_subset_pairs_to_be_tested 12 |> printf "%d\n"

  (* 21384
     0.456ms *)
end

include Solution.Make (M)
