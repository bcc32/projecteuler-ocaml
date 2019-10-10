open! Core
open! Import

(* In order for a pair of sets to satisfy rule 2 but not rule 2, they need to be
   of the same size, since rule 2 implies that the sums of the sets are not
   equal.

   For any pair of disjoint equal-size sets (L, R), where WLOG min(L) < min(R),
   if L is "dominated" by R, then we know sum(L) < sum(R).  I.e., if l_1 < r_1,
   l_2 < r_2, ... then clearly l_1 + l_2 + ... < r_1 + r_2 + ... *)

(* First, we pick the set of 2k elements from which to draw our two k-sets. *)
let count_subset_pair_unions ~n ~k = Number_theory.Int.binomial n (2 * k)

exception Not_dominating

type which_set =
  | Left
  | Right
[@@deriving compare, sexp_of]

(* Count the number of permutations of k copies each of (L, R) where L is
   "dominated" by R. *)
let count_non_dominating_set_pairs ~k =
  let seq = List.init k ~f:(fun _ -> Left) @ List.init k ~f:(fun _ -> Right) in
  Sequences.permutations seq ~compare:[%compare: which_set]
  |> Sequence.take_while ~f:(fun perm -> Poly.( = ) (List.hd_exn perm) Left)
  |> Sequence.count ~f:(fun perm ->
    let non_dominating =
      try
        let rec loop perm left_minus_right_in_prefix =
          match perm with
          | [] -> false
          | Left :: tl -> loop tl (left_minus_right_in_prefix + 1)
          | Right :: tl ->
            let left_minus_right_in_prefix = left_minus_right_in_prefix - 1 in
            if left_minus_right_in_prefix < 0
            then Exn.raise_without_backtrace Not_dominating;
            loop tl left_minus_right_in_prefix
        in
        loop perm 0
      with
      | Not_dominating -> true
    in
    if debug && non_dominating
    then Debug.eprint_s [%message "non-dominating" (perm : which_set list)];
    non_dominating)
;;

(* Count the total number of pairs of disjoint subsets from a set of [n]
   elements that need to be tested for rule 1. *)
let count_subset_pairs_to_be_tested ~n =
  List.range 1 (n / 2) ~stop:`inclusive
  |> List.sum
       (module Int)
       ~f:(fun k ->
         let a = count_subset_pair_unions ~n ~k in
         let b = count_non_dominating_set_pairs ~k in
         if debug
         then Debug.eprint_s [%message "" (k : int) (a : int) (b : int) (a * b : int)];
         a * b)
;;

let main () = count_subset_pairs_to_be_tested ~n:12 |> printf "%d\n"

(* 0.456ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 21384 |}]
;;

include (val Solution.make ~problem:(Number 106) ~main)
