open! Core
open! Import

let is_pythagorean_triple a b c = (a * a) + (b * b) = c * c

let iter_all_pythagorean_triples ~with_hypotenuse_less_than:max_hypot ~f =
  for m = 2 to Number_theory.Int.isqrt max_hypot + 1 do
    let m'2 = m * m in
    for n = 1 to m - 1 do
      if Number_theory.Int.gcd m n = 1 && (m mod 2 = 0 || n mod 2 = 0)
      then (
        let n'2 = n * n in
        let mn = 2 * m * n in
        let rec loop k =
          let c = k * (m'2 + n'2) in
          if c > max_hypot
          then ()
          else (
            let a = k * (m'2 - n'2) in
            let b = k * mn in
            f a b c;
            loop (k + 1))
        in
        loop 1)
    done
  done
;;

(* FIXME this is buggy, doesn't generate in the correct order *)
let pythagorean_triples =
  let open Sequence.Let_syntax in
  let triples_unsorted =
    let%map m, n =
      let%bind m = Number_theory.Int.natural_numbers () ~init:2 in
      let%bind n = Sequence.range 1 m in
      if Number_theory.Int.gcd m n = 1 && (m mod 2 = 0 || n mod 2 = 0)
      then return (m, n)
      else Sequence.empty
    in
    let a = (m * m) - (n * n) in
    let b = 2 * m * n in
    let c = (m * m) + (n * n) in
    let%bind k = Number_theory.Int.natural_numbers () ~init:1 in
    return (k * a, k * b, k * c)
  in
  Sequence.interleave triples_unsorted
;;
