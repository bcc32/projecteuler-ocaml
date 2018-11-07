open! Core
open! Import

let rec power_set list =
  match list with
  | [] -> [ [] ]
  | hd :: tl ->
    power_set tl |> List.concat_map ~f:(fun subset -> [ subset; hd :: subset ])
;;

let%expect_test _ =
  power_set [ 1; 2; 3 ] |> [%sexp_of: int list list] |> print_s;
  [%expect {| (() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)) |}]
;;

let is_special list =
  let subsets = power_set list |> List.tl_exn in
  let subsets = Array.of_list subsets in
  let sum subset = List.sum (module Int) ~f:Fn.id subset in
  with_return (fun { return } ->
    for i = 0 to Array.length subsets - 1 do
      for j = i + 1 to Array.length subsets - 1 do
        let b, c = subsets.(i), subsets.(j) in
        let sb, sc = sum b, sum c in
        let lb, lc = List.length b, List.length c in
        if not (sb <> sc && lb > lc ==> (sb > sc) && lc > lb ==> (sc > sb))
        then return false
      done
    done;
    true)
;;

let%test _ = is_special [ 2; 3; 4 ]
let%test _ = not (is_special [ 2; 3; 5 ])

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
  |> Sequence.filter ~f:is_special
  |> Sequence.min_elt ~compare:compare_by_sum
  |> uw
;;

let%expect_test _ =
  let limit = 4 in  (* change to 6 to see the example from the problem *)
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

module M = struct
  let problem = Number 103

  let main () =
    optimum_special_set 7 ~ubound:45
    |> List.rev_map ~f:Int.to_string
    |> String.concat
    |> print_endline
  ;;

  (* 20313839404245
     4.60712m *)
end

include Solution.Make (M)
