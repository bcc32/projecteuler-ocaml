open! Core
open! Import

module M = struct
  let problem = `Number 20

  let main () =
    Bigint.of_int 100
    |> Number_theory.Bigint.factorial
    |> Number_theory.Bigint.sum_digits
    |> printf !"%{Bigint}\n"
  ;;
end

include Solution.Make(M)
