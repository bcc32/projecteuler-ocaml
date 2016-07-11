open Core.Std
open Bignum.Std

module M = struct
  let problem_number = 20

  let main () =
    Euler.factorial 100
    |> Euler.sum_digits
    |> Bigint.to_int_exn
    |> printf "%d\n"
end

include Solution.Make(M)
