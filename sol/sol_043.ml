open Core

module M = struct
  let problem_number = 43

  let check_digits d =
    let is_div d1 d2 d3 p =
      (100 * d1 + 10 * d2 + d3) mod p = 0
    in
    match d with
    | [_d1; d2; d3; d4; d5; d6; d7; d8; d9; d10] ->
      is_div d2 d3 d4 2
      && is_div d3 d4 d5 3
      && is_div d4 d5 d6 5
      && is_div d5 d6 d7 7
      && is_div d6 d7 d8 11
      && is_div d7 d8 d9 13
      && is_div d8 d9 d10 17
    | _ -> invalid_arg "wrong number of digits"

  let main () =
    List.range 0 10
    |> Euler.permutations ~cmp:Int.compare
    |> Sequence.filter ~f:check_digits
    |> Sequence.sum (module Int)
         ~f:(fun d -> Euler.Int.int_of_digits (Sequence.of_list d))
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
