open! Core
open Bignum.Std

module M = struct
  let problem_number = 48

  let ten_ten =
    let ten = Bigint.of_int 10 in
    Bigint.pow ten ten

  let main () =
    let sum =
      Sequence.range ~stop:`inclusive 1 1000
      |> Sequence.sum (module Bigint) ~f:(fun n ->
        let n = Bigint.of_int n in
        Bigint.pow n n
      )
    in
    Bigint.(sum % ten_ten)
    |> Bigint.to_int_exn
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
