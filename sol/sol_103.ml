open! Core
open! Import

module Subset = struct
  type 'a t =
    { mask : int
    ; elements : 'a array
    }

  let to_array { mask; elements } =
    Array.filteri elements ~f:(fun i _ -> mask land (1 lsl i) <> 0)
  ;;

  let sexp_of_t sexp_of_a = to_array >> [%sexp_of: a array]

  let power_set elements =
    List.init (1 lsl Array.length elements) ~f:(fun mask -> { mask; elements })
  ;;

  let sum (type a) (module M : Container.Summable with type t = a) { mask; elements } ~f =
    Array.foldi elements ~init:M.zero ~f:(fun i acc elt ->
      if mask land (1 lsl i) <> 0 then M.( + ) acc (f elt) else acc)
  ;;

  let length t = Int.popcount t.mask
end

let%expect_test _ =
  Subset.power_set [| 1; 2; 3 |] |> [%sexp_of: int Subset.t list] |> print_s;
  [%expect {| (() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)) |}]
;;

let is_special elements =
  let subsets = Subset.power_set elements |> List.tl_exn in
  let subset_sums_and_lengths =
    Array.of_list_map subsets ~f:(fun s ->
      Subset.sum (module Int) s ~f:Fn.id, Subset.length s)
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

let%test _ = is_special [| 2; 3; 4 |]
let%test _ = not (is_special [| 2; 3; 5 |])

let rec sets n ~ubound =
  if n = 0
  then Sequence.singleton []
  else
    Sequence.range 1 ubound ~stop:`inclusive
    |> Sequence.concat_map ~f:(fun x ->
      sets (n - 1) ~ubound:(x - 1) |> Sequence.map ~f:(fun set -> x :: set))
;;

let optimum_special_set n ~ubound =
  let compare_by_sum = Comparable.lift Int.compare ~f:(List.sum (module Int) ~f:Fn.id) in
  sets n ~ubound
  |> Sequence.filter ~f:(is_special << Array.of_list)
  |> Sequence.min_elt ~compare:compare_by_sum
  |> uw
;;

let%expect_test _ =
  let limit = 4 in
  (* change to 6 to see the example from the problem *)
  for n = 1 to limit do
    print_s [%message "" (n : int) (optimum_special_set n ~ubound:30 : int list)]
  done;
  [%expect
    {|
    ((n 1) ("optimum_special_set n ~ubound:30" (1)))
    ((n 2) ("optimum_special_set n ~ubound:30" (2 1)))
    ((n 3) ("optimum_special_set n ~ubound:30" (4 3 2)))
    ((n 4) ("optimum_special_set n ~ubound:30" (7 6 5 3))) |}]
;;

let main () =
  optimum_special_set 7 ~ubound:45
  |> List.rev_map ~f:Int.to_string
  |> String.concat
  |> print_endline
;;

(* 20313839404245
   4.60712m *)

include (val Solution.make ~problem:(Number 103) ~main)
