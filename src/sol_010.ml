open Core.Std

module M = struct
  let problem_number = 10

  let main () =
    Euler.primes
    |> Sequence.take_while ~f:((>) 2_000_000)
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
end

include Solution.Make(M)
