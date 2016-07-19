open Core.Std
open Bignum.Std

module M = struct
  let problem_number = 25

  let main () =
    Sequence.findi Euler.fibonacci ~f:(fun _ f ->
      String.length (Bigint.to_string f) >= 1000
    )
    |> Option.value_exn
    |> fst
    |> succ
    |> printf "%d\n"
end

include Solution.Make(M)
