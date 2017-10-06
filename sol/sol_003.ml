open! Core

module M = struct
  let problem_number = 3

  let main () =
    Euler.Int.factor 600851475143
    |> List.last_exn
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
