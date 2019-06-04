open! Core
open! Import

let problem = Number 613

let main () =
  let open Float.O in
  Numerics.Float.integrate () ~lo:0. ~hi:40. ~intervals:32_000 ~f:(fun x ->
    let max_y = 30. - (x * 0.75) in
    Numerics.Float.integrate () ~lo:0. ~hi:max_y ~intervals:32_000 ~f:(fun y ->
      let to_southeast = Float.atan2 (-y) (40. - x) in
      let to_northwest = Float.atan2 (30. - y) (-x) in
      to_northwest - to_southeast))
  / (Float.pi * 30. * 40.)
  |> printf "%.10f\n"
;;

(* 0.3916721504
   6.56162m *)

include (val Solution.make ~problem ~main)
