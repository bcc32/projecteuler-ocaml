open Core
open Bignum.Std

module M = struct
  let problem_number = 56

  let main () =
    let hundred = Bigint.of_int 100 in
    let range = Euler.Bigint.range Bigint.one hundred in
    Sequence.cartesian_product range range
    |> Sequence.map ~f:(fun (a, b) ->
      Bigint.pow a b
      |> Euler.Bigint.sum_digits)
    |> Sequence.max_elt ~cmp:Bigint.compare
    |> Option.value_exn
    |> printf !"%{Bigint}\n"
end

include Euler.Solution.Make(M)
