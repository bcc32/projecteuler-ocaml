open Core
open Bignum.Std

module M = struct
  let problem_number = 20

  let main () =
    Bigint.of_int 100
    |> Euler.Bigint.factorial
    |> Euler.Bigint.sum_digits
    |> printf !"%{Bigint}\n"
end

include Solution.Make(M)
