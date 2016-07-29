open Core.Std

module M = struct
  let problem_number = 41

  let main () =
    Sequence.range ~stride:(-1) 9 0
    |> Sequence.find_map ~f:(fun n ->
      List.range ~stop:`inclusive 1 n
      |> Euler.permutations ~cmp:Int.descending
      |> Sequence.find_map ~f:(fun p ->
        let n =
          Sequence.of_list p
          |> Euler.int_of_digits
        in
        Option.some_if (Euler.is_prime n) n
      )
    )
    |> uw
    |> printf "%d\n"
end

include Solution.Make(M)
