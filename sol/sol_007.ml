open! Core

module M = struct
  let problem_number = 7

  let main () =
    Sequence.nth_exn Euler.Int.primes 10000 (* 0-index *)
    |> printf "%d\n"
end

include Euler.Solution.Make(M)