open! Core
open Bignum.Std

module M = struct
  let problem_number = 53

  let main () =
    let limit = Bigint.of_int 1_000_000 in
    Sequence.(
      range 1 100 ~stop:`inclusive
      |> map ~f:(fun n ->
        range 0 n ~stop:`inclusive
        |> map ~f:(fun r -> n, r))
      |> concat
      |> map ~f:(fun (n, r) ->
        let n = Bigint.of_int n in
        let r = Bigint.of_int r in
        Euler.Bigint.binomial n r)
      |> count ~f:(Bigint.(<) limit))
    |> printf "%d\n"
end

include Euler.Solution.Make(M)