open Core.Std

module M = struct
  let problem_number = 30

  (* Derived by observing that 9^5 * n < 10^(n - 1) for all n > 6. *)
  let max_digits = 6

  let main () =
    Sequence.range 2 (Int.pow 10 max_digits)
    |> Sequence.filter ~f:(fun n ->
      Euler.digits_of_int n
      |> List.sum (module Int) ~f:(Fn.flip Int.pow 5)
      |> (=) n
    )
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
end

include Solution.Make(M)