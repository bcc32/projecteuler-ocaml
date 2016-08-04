open Core.Std
open Bignum.Std

module M = struct
  let problem_number = 56

  let main () =
    let range = Sequence.range 1 100 in
    Sequence.cartesian_product range range
    |> Sequence.map ~f:(fun (a, b) ->
      Bigint.(pow (of_int a) (of_int b))
      |> Euler.sum_digits
    )
    |> Sequence.max_elt ~cmp:Bigint.compare
    |> Option.value_exn
    |> printf !"%{Bigint}\n"
end

include Solution.Make(M)
