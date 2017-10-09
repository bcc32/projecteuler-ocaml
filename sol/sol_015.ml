open! Core
open! Import
open Bignum.Std

module M = struct
  let problem = `Number 15

  let main () =
    let n = Bigint.of_int 40 in
    let r = Bigint.of_int 20 in
    Number_theory.Bigint.binomial n r
    |> printf !"%{Bigint}\n"
  ;;
end

include Solution.Make(M)
