open! Core

let quadratic_formula a b c =
  let open Float.O in
  let determinant = b ** 2. - 4. * a * c in
  match Float.sign_or_nan determinant with
  | Neg | Nan -> `None
  | Zero -> `One ((-b) / (2. * a))
  | Pos ->
    let d = sqrt determinant in
    let x0 = (-b - d) / (2. * a) in
    let x1 = (-b + d) / (2. * a) in
    match Float.sign_exn a with
     | Zero -> assert false
     | Neg -> `Two (x1, x0)
     | Pos -> `Two (x0, x1)
;;
