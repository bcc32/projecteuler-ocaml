open! Core
open! Import

let problem = Number 205

type dist = Percent.t Map.M(Int).t

let empty_dist : dist = Map.of_alist_exn (module Int) [ 0, Percent.of_mult 1.0 ]

let shift_n dist ~n : dist =
  let comparator = Map.comparator dist in
  Map.to_alist dist
  |> List.map ~f:(Tuple2.map_fst ~f:(( + ) n))
  |> Map.Using_comparator.of_alist_exn ~comparator
;;

let scale_div_n dist ~n : dist =
  Map.map dist ~f:(fun p -> Percent.scale p (1. /. float n))
;;

let merge_dist ~key:_ data =
  let prob =
    match data with
    | `Both (p1, p2) -> Percent.(p1 + p2)
    | `Left p1 -> p1
    | `Right p2 -> p2
  in
  Some prob
;;

let add_die dist die : dist =
  Sequence.range 1 die ~stop:`inclusive
  |> Sequence.map ~f:(fun n -> dist |> shift_n ~n |> scale_div_n ~n:die)
  |> Sequence.fold ~init:(Map.empty (module Int)) ~f:(Map.merge ~f:merge_dist)
;;

let dice_set ~faces ~len : dist =
  Sequence.range 0 len
  |> Sequence.fold ~init:empty_dist ~f:(fun dist _ -> add_die dist faces)
;;

let win_prob winner loser : Percent.t =
  Map.mapi winner ~f:(fun ~key:roll_w ~data:prob_w ->
    Map.mapi loser ~f:(fun ~key:roll_l ~data:prob_l ->
      if roll_w > roll_l then Percent.(prob_w * prob_l) else Percent.zero)
    |> Map.data
    |> List.sum (module Percent) ~f:Fn.id)
  |> Map.data
  |> List.sum (module Percent) ~f:Fn.id
;;

let main () =
  let peter = dice_set ~faces:4 ~len:9 in
  let colin = dice_set ~faces:6 ~len:6 in
  win_prob peter colin |> Percent.to_mult |> printf "%.07f\n"
;;

(* 0.5731441
   0.168ms *)

include (val Solution.make ~problem ~main)
