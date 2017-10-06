open Core

module M = struct
  let problem_number = 52

  let same_digits n =
    let sort_digits n =
      Euler.Int.digits_of_int n
      |> List.sort ~cmp:Int.compare
    in
    let n_digits = sort_digits n in
    List.range 2 6 ~stop:`inclusive
    |> List.map ~f:(( * ) n)
    |> List.map ~f:sort_digits
    |> List.for_all ~f:(List.equal ~equal:Int.equal n_digits)

  let main () =
    Euler.Int.natural_numbers ~init:1 ()
    |> Sequence.find ~f:same_digits
    |> Option.value_exn
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
