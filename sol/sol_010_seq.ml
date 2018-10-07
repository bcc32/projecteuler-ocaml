open! Core
open! Import

module M = struct
  let problem =
    Custom { number = 10; tag = "seq"; description = "using primes Sequence.t" }
  ;;

  let main () =
    Number_theory.Int.primes
    |> Sequence.take_while ~f:(fun x -> x < 2_000_000)
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
