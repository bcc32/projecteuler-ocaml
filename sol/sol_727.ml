open! Core
open! Import

module Givens = struct
  type t =
    { ra : float
    ; rb : float
    ; rc : float
    }
  [@@deriving sexp_of]

  let all =
    lazy
      (Sequence.to_list
         (let open Sequence.Let_syntax in
          let%bind ra = Sequence.range 1 100 ~stop:`inclusive in
          let%bind rb = Sequence.range (ra + 1) 100 ~stop:`inclusive in
          let%bind rc = Sequence.range (rb + 1) 100 ~stop:`inclusive in
          if List.reduce_exn [ ra; rb; rc ] ~f:Number_theory.Int.gcd <> 1
          then Sequence.empty
          else return { ra = float ra; rb = float rb; rc = float rc }))
  ;;

  (* 3-4-5 right triangle *)
  let example = { ra = 1.; rb = 2.; rc = 3. }
end

(* Let B be the origin, and let C lie on the x-axis.

   We find the coordinates of D using trigonometry.

   D lies at coordinates (rb, rd). *)

let rd ({ ra; rb; rc } : Givens.t) =
  let open Float.O in
  let ab = ra + rb in
  let bc = rb + rc in
  let ac = ra + rc in
  let cos_ABC = ((ab * ab) + (bc * bc) - (ac * ac)) / (2. * ab * bc) in
  (* <ABC and <FDG are supplementary angles. *)
  let cos_FDG = -cos_ABC in
  (* 2rb^2(1 - cos ABC) = 2rd^2(1 - cos FDG) *)
  rb * sqrt ((1. - cos_ABC) / (1. - cos_FDG))
;;

let%expect_test "rd" =
  print_s [%sexp (rd Givens.example : float)];
  [%expect {| 1 |}]
;;

(* Use Descartes' Theorem: https://en.wikipedia.org/wiki/Descartes%27_theorem *)
let re ({ ra; rb; rc } : Givens.t) =
  let open Float.O in
  let ka = 1. / ra in
  let kb = 1. / rb in
  let kc = 1. / rc in
  let ke = ka + kb + kc + (2. * sqrt ((ka * kb) + (kb * kc) + (kc * ka))) in
  1. / ke
;;

(* Find a point whose distance from B is rb + re and whose distance from C is rc + re.

   {v
     x^2 + y^2 = (rb + re)^2                                 (1)
     (x - (rb + rc))^2 + y^2 = (rc + re)^2                   (2)
     x^2 - 2(rb + rc)x + (rb + rc)^2 + y^2 = (rc + re)^2     (2)
     2(rb + rc)x = (rb + re)^2 + (rb + rc)^2 - (rc + re)^2   (1) - (2)
     x = ((rb + re)^2 + (rb + rc)^2 - (rc + re)^2) / (2(rb + rc))
   v} *)
let coord_e ({ ra = _; rb; rc } as givens : Givens.t) =
  let open Float.O in
  let re = re givens in
  let x =
    (((rb + re) ** 2.) + ((rb + rc) ** 2.) - ((rc + re) **. 2.)) / (2. * (rb + rc))
  in
  let y = sqrt (((rb + re) * (rb + re)) - (x * x)) in
  x, y
;;

let%expect_test "e" =
  print_s [%sexp (re Givens.example : float)];
  [%expect {| 0.2608695652173913 |}];
  print_s [%sexp (coord_e Givens.example : float * float)];
  [%expect {| (1.9478260869565218 1.1478260869565213) |}]
;;

let d ({ ra = _; rb; rc = _ } as givens : Givens.t) =
  let open Float.O in
  let xd = rb in
  let yd = rd givens in
  let xe, ye = coord_e givens in
  Float.hypot (xd - xe) (yd - ye)
;;

let main () =
  let list = force Givens.all in
  let expected =
    let open Float.O in
    List.sum (module Float) list ~f:d / float (List.length list)
  in
  printf "%.8f\n" expected
;;

let%expect_test "answer" =
  main ();
  [%expect {| 3.64039141 |}]
;;

include (val Solution.make ~problem:(Number 727) ~main)
