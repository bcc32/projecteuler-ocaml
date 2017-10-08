open! Core
open Bignum.Std

module M = struct
  let problem = `Number 15

  let main () =
    let n = Bigint.of_int 40 in
    let r = Bigint.of_int 20 in
    Euler.Bigint.binomial n r
    |> printf !"%{Bigint}\n"
  ;;
end

include Euler.Solution.Make(M)
