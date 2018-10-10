open! Core
open! Import

module M = struct
  let problem = Number 25
  let threshold = Bigint.(pow (of_int 10) (of_int 999))

  let main () =
    Sequence.findi Number_theory.Bigint.fibonacci ~f:(fun _ f -> Bigint.(f >= threshold))
    |> uw
    |> fst
    |> printf "%d\n"
  ;;

  (* 4782
     1.692ms *)
end

include Solution.Make (M)
