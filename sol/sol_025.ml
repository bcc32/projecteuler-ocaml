open! Core
open! Import

module M = struct
  let problem = `Number 25

  let main () =
    Sequence.findi Number_theory.Bigint.fibonacci ~f:(fun _ f ->
      String.length (Bigint.to_string f) >= 1000)
    |> uw
    |> fst
    |> printf "%d\n"
  ;;
  (* 4782
     37ms *)
end

include Solution.Make(M)
