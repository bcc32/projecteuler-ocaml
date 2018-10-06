open! Core
open! Import

module M = struct
  let problem = `Number 613

  let main () =
    let open Float.O in
    Numerics.Float.integrate () ~low:0. ~high:40. ~intervals:32_000 ~f:(fun x ->
      let max_y = 30. - (x * 0.75) in
      Numerics.Float.integrate () ~low:0. ~high:max_y ~intervals:32_000 ~f:(fun y ->
        let to_southeast = Float.atan2 (-y) (40. - x) in
        let to_northwest = Float.atan2 (30. - y) (-x) in
        to_northwest - to_southeast))
    / (Float.pi * 30. * 40.)
    |> printf "%.10f\n"
  ;;

  (* 0.3916721504
     6.56162m *)
end

include Solution.Make (M)
