open! Core
open Bignum.Std

module M = struct
  let problem = `Number 20

  let main () =
    Bigint.of_int 100
    |> Euler.Bigint.factorial
    |> Euler.Bigint.sum_digits
    |> printf !"%{Bigint}\n"
  ;;
end

include Euler.Solution.Make(M)
