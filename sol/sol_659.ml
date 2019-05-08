open! Core
open! Import

let primes =
  Number_theory.Int.primes
  |> Fn.flip Sequence.drop_eagerly 1
  |> Fn.flip Sequence.take 10_000
  |> Sequence.to_list
;;

(* Find the largest prime that divides two consecutive elements of the sequence [1^2+m,
   2^2+m, ...]. *)
let find_largest_prime m =
  primes
  |> List.filter ~f:(fun p ->
    let k = p / 2 in
    ((k * k) + m) % p = 0)
  |> [%sexp_of: int list]
  |> print_s
;;

let main () =
  for i = 1 to 20 do
    find_largest_prime (i * i)
  done
;;

include (val Solution.make ~problem:(Number 659) ~main)
