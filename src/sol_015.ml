open Core.Std
open Bignum.Std

module M = struct
  let problem_number = 15

  let main () =
    Euler.binomial 40 20
    |> printf !"%{Bigint}\n"
end

include Solution.Make(M)
