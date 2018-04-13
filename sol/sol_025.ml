open! Core
open! Import

module M = struct
  let problem = `Number 25

  let main () =
    Sequence.findi Number_theory.Bigint.fibonacci ~f:(fun _ f ->
      String.length (Bigint.to_string f) >= 1000)
    |> Option.value_exn
    |> Tuple2.get1
    |> succ
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
