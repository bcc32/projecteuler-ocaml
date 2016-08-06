open Core.Std

module M = struct
  let problem_number = 41

  let main () =
    Sequence.range ~stride:(-1) 9 0
    |> Sequence.find_map ~f:(fun n ->
      List.range 1 n ~stop:`inclusive
      |> Euler.permutations ~cmp:Int.descending
      |> Sequence.find_map ~f:(fun p ->
        let n =
          Sequence.of_list p
          |> Euler.Int.int_of_digits
        in
        Option.some_if (Euler.Int.is_prime n) n))
    |> Option.value_exn
    |> printf "%d\n"
end

include Solution.Make(M)
