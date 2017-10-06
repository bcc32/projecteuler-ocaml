open Core

module M = struct
  let problem_number = 10

  let main () =
    Euler.prime_sieve 2_000_000
    |> Array.foldi ~init:0 ~f:(fun n acc is_prime ->
      if is_prime
      then acc + n
      else acc)
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
