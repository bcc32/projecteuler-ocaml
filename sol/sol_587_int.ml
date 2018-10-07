open! Core
open! Import

module M = struct
  let problem = Custom { number = 587; tag = "int"; description = "integration" }

  (* fix the circle radius to be 1 *)
  let l_section_area = Float.(1. - (pi / 4.))
  let area_threshold = Percent.apply (Percent.of_bp_int 10) l_section_area

  let height n x =
    let open Float.O in
    let y_line = x / float n in
    (* (x - 1)^2 + (y - 1)^2 = 1 *)
    let y_circle = 1. - sqrt (1. - ((x - 1.) ** 2.)) in
    Float.min y_line y_circle
  ;;

  let concave_triangle_area n =
    Numerics.Float.integrate () ~f:(height n) ~low:0. ~high:1. ~intervals:1000
  ;;

  let main () =
    Number_theory.Int.natural_numbers () ~init:1
    |> Sequence.find_exn ~f:(fun n -> Float.(concave_triangle_area n < area_threshold))
    |> printf "%d\n"
  ;;

  (* 2240 354ms *)
end

include Solution.Make (M)
