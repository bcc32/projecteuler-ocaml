open! Core

let quadratic_formula a b c =
  let open Float.O in
  let determinant = b ** 2. - 4. * a * c in
  match Float.sign_or_nan determinant with
  | Neg | Nan -> `None
  | Zero -> `One ((-b) / (2. * a))
  | Pos ->
    let d = sqrt determinant in
    `Two ((-b - d) / (2. * a), (-b + d) / (2. * a))
;;
