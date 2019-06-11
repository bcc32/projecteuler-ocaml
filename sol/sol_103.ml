open! Core
open! Import

module Subset = struct
  module Array = Array.Permissioned

  type t =
    { mask : int
    ; elements : (int, read) Array.t
    }

  let to_array { mask; elements } =
    Array.filteri elements ~f:(fun i _ -> mask land (1 lsl i) <> 0)
  ;;

  let sexp_of_t sexp_of_a = to_array >> [%sexp_of: (a, read) Array.t]

  let power_set elements =
    List.init (1 lsl Array.length elements) ~f:(fun mask -> { mask; elements })
  ;;

  let sum { mask; elements } =
    let sum = ref 0 in
    for i = 0 to Array.length elements - 1 do
      sum := !sum + (elements.(i) * ((mask asr i) land 1))
    done;
    !sum
  ;;

  let length t = Int.popcount t.mask
end

let%expect_test _ =
  Subset.power_set (Array.Permissioned.of_array_id [| 1; 2; 3 |])
  |> [%sexp_of: int Subset.t list]
  |> print_s;
  [%expect {| (() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)) |}]
;;

let is_special elements =
  let subset_sums_and_lengths =
    Subset.power_set elements
    |> List.tl_exn
    |> Array.of_list_map ~f:(fun s -> Subset.sum s, Subset.length s)
  in
  with_return (fun { return } ->
    for i = 0 to Array.length subset_sums_and_lengths - 1 do
      for j = i + 1 to Array.length subset_sums_and_lengths - 1 do
        let sb, lb = subset_sums_and_lengths.(i) in
        let sc, lc = subset_sums_and_lengths.(j) in
        if not (sb <> sc && lb > lc ==> (sb > sc) && lc > lb ==> (sc > sb))
        then return false
      done
    done;
    true)
;;

let%test _ = is_special (Array.Permissioned.of_array_id [| 2; 3; 4 |])
let%test _ = not (is_special (Array.Permissioned.of_array_id [| 2; 3; 5 |]))

let rec iter_sets n array ~ubound ~f =
  if n = 0
  then f (array :> (int, read) Array.Permissioned.t)
  else
    for x = 1 to ubound do
      Array.Permissioned.set array (n - 1) x;
      iter_sets (n - 1) array ~ubound:(x - 1) ~f
    done
;;

let optimum_special_set n ~ubound =
  let min_set_by_sum = ref None in
  let min_sum = ref Int.max_value in
  iter_sets n (Array.Permissioned.create 0 ~len:n) ~ubound ~f:(fun set ->
    if debug then Debug.eprint_s [%sexp (set : (int, read) Array.Permissioned.t)];
    if is_special set
    then (
      if debug then Debug.eprint "special";
      let sum = Array.Permissioned.sum (module Int) set ~f:Fn.id in
      if sum < !min_sum
      then (
        min_set_by_sum := Some (Array.Permissioned.to_array set);
        min_sum := sum)));
  Option.value_exn !min_set_by_sum
;;

let%expect_test "example" =
  let limit = 4 in
  (* change to 6 to see the example from the problem *)
  for n = 1 to limit do
    print_s [%message "" (n : int) (optimum_special_set n ~ubound:30 : int array)]
  done;
  [%expect
    {|
    ((n 1) ("optimum_special_set n ~ubound:30" (1)))
    ((n 2) ("optimum_special_set n ~ubound:30" (1 2)))
    ((n 3) ("optimum_special_set n ~ubound:30" (2 3 4)))
    ((n 4) ("optimum_special_set n ~ubound:30" (3 5 6 7))) |}]
;;

let main () =
  optimum_special_set 7 ~ubound:45
  |> Array.to_list
  |> List.map ~f:Int.to_string
  |> String.concat
  |> print_endline
;;

(* 20313839404245
   4.60712m *)

include (val Solution.make ~problem:(Number 103) ~main)
