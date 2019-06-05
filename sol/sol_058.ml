open! Core
open! Import

(* Interpolate the quadratic polynomial [ax^2 + bx + c] passing through points
   [(1, y1), (2, y2), (3, y3)] using the Lagrange interpolation formula. *)
let interpolate y1 y2 y3 =
  stage (fun x ->
    (y1 * (x - 2) * (x - 3) / ((1 - 2) * (1 - 3)))
    + (y2 * (x - 1) * (x - 3) / ((2 - 1) * (2 - 3)))
    + (y3 * (x - 1) * (x - 2) / ((3 - 1) * (3 - 2))))
;;

let main () =
  let top_right = unstage (interpolate 3 13 31) in
  let top_left = unstage (interpolate 5 17 37) in
  let bottom_left = unstage (interpolate 7 21 43) in
  Number_theory.Int.natural_numbers () ~init:1
  |> Sequence.folding_map ~init:(0, 1) ~f:(fun (primes, total) r ->
    let side_length = (2 * r) + 1 in
    let primes =
      primes
      + ([ top_right r; top_left r; bottom_left r ]
         |> List.count ~f:Number_theory.Int.is_prime)
    in
    let total = total + 4 in
    (primes, total), (side_length, (primes, total)))
  |> Sequence.find_exn ~f:(fun (_, (primes, total)) -> primes * 10 < total)
  |> fst
  |> printf "%d\n"
;;

(* 26241
   337.116ms *)
include (val Solution.make ~problem:(Number 58) ~main)
