open! Core
open! Import

module M = struct
  let problem = `Number 293

  let is_prime = lazy (Number_theory.prime_sieve 100_000)

  let is_admissible n =
    let rec start n i =
      if n = 1
      then true
      else if (force is_prime).(i)
      then (
        if n mod i <> 0
        then false
        else (continue n i))
      else (start n (Number_theory.Int.next_probable_prime i))
    and continue n i =
      if n mod i = 0
      then (continue (n / i) i)
      else (start n (i + 1))
    in
    start n 2
  ;;

  let main () =
    Number_theory.Int.natural_numbers ~init:1 ()
    |> Sequence.take_while ~f:(fun x -> x < 1_000_000_000)
    |> Sequence.filter ~f:is_admissible
    |> Sequence.length
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
