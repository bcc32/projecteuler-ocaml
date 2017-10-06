open Core

module M = struct
  let problem_number = 24

  let main () =
    let permutations =
      List.range 0 10
      |> Euler.permutations ~cmp:Int.compare
    in
    Sequence.nth_exn permutations 999999
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:""
    |> printf "%s\n"
end

include Euler.Solution.Make(M)
