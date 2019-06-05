open! Core
open! Import

(* fix the circle radius to be 1 *)
let l_section_area = Float.(1. - (pi / 4.))
let area_threshold = Percent.apply (Percent.of_bp_int 10) l_section_area

(* see diagram for notation *)
let concave_triangle_area n =
  let open Float.O in
  let theta = Float.atan2 1. (float n) in
  let tan_theta = 1. / float n in
  let x_d =
    let a = 1. + (tan_theta ** 2.) in
    let b = -2. * (1. + tan_theta) in
    let c = 1. in
    match Algebra.quadratic_formula a b c with
    | `None | `One _ -> assert false
    | `Two (x_d, _) -> x_d
  in
  let phi = Float.asin (1. - x_d) in
  let a_ABC = 0.5 * Float.sin theta in
  let a_ODB = phi / 2. in
  let a_ODC = 0.5 * (1. - Float.sin theta) * Float.sin phi in
  a_ABC - (a_ODB - a_ODC)
;;

let main () =
  let open Float.O in
  Number_theory.Int.natural_numbers () ~init:1
  |> Sequence.find_exn ~f:(fun n -> concave_triangle_area n < area_threshold)
  |> printf "%d\n"
;;

(* 2240
   764us *)

include (val Solution.make ~problem:(Number 587) ~main)
