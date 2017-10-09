open! Core
open! Import

module M = struct
  let problem = `Custom (10, `Key "seq", `Description "using primes Sequence.t")

  let main () =
    Number_theory.Int.primes
    |> Sequence.take_while ~f:((>) 2_000_000)
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
