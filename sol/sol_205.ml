open! Core
open! Import

module M = struct
  let problem = Number 205

  type dist = Percent.t Int.Map.t

  let empty_dist : dist = Int.Map.of_alist_exn [ 0, Percent.of_mult 1.0 ]

  let shift_n dist ~n : dist =
    let comparator = Map.comparator dist in
    Map.to_alist dist
    |> List.map ~f:(Tuple2.map_fst ~f:(( + ) n))
    |> Map.Using_comparator.of_alist_exn ~comparator
  ;;

  let scale_div_n dist ~n : dist =
    let factor = Percent.of_mult (1.0 /. Float.of_int n) in
    Map.map dist ~f:Percent.(( * ) factor)
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
    |> Sequence.fold ~init:Int.Map.empty ~f:(Map.merge ~f:merge_dist)
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
end

include Solution.Make (M)
