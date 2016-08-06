open Core.Std
open Bignum.Std

module M = struct
  let problem_number = 53

  let main () =
    Sequence.(
      range 1 100 ~stop:`inclusive
      |> map ~f:(fun n ->
        range 0 n ~stop:`inclusive
        |> map ~f:(fun r -> n, r))
      |> concat
      |> map ~f:(fun (n, r) -> Euler.binomial n r)
      |> count ~f:(Bigint.((<) (of_int 1_000_000))))
    |> printf "%d\n"
end

include Solution.Make(M)
