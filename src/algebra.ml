open! Core
open! Import

let ( >> ) x y = Float.(x / y > 1000.)
let sort_roots x y = if Float.(x < y) then `Two (x, y) else `Two (y, x)

(* Formula taken from p. 162-3 of https://cr.yp.to/2005-590/goldberg.pdf.

   David Goldberg. 1991. What every computer scientist should know about floating-point
   arithmetic. ACM Comput. Surv. 23, 1 (March 1991), 5-48. DOI:
   https://doi.org/10.1145/103162.103163. *)
let quadratic_formula a b c =
  let open Float.O in
  let determinant = (b * b) - (4. * a * c) in
  match Float.sign_or_nan determinant with
  | Neg | Nan -> `None
  | Zero -> `One (-b / (2. * a))
  | Pos ->
    let d = sqrt determinant in
    if b * b >> abs (a * c)
    then
      if b > 0.
      then sort_roots ((-b - d) / (2. * a)) (2. * c / (-b - d))
      else sort_roots (2. * c / (-b + d)) ((-b + d) / (2. * a))
    else (
      let x0 = (-b - d) / (2. * a) in
      let x1 = (-b + d) / (2. * a) in
      sort_roots x0 x1)
;;
