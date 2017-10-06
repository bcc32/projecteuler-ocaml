open Core

module M = struct
  let problem_number = 1

  let threes_and_fives =
    List.range 1 1000
    |> List.filter ~f:(fun x -> x mod 3 = 0 || x mod 5 = 0)
    |> List.fold ~init:0 ~f:(+)

  let main () =
    printf "%d\n" threes_and_fives
end

include Euler.Solution.Make(M)
