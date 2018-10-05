open! Core
open! Import

module M = struct
  let problem = `Number 24

  let main () =
    let permutations = List.range 0 10 |> Sequences.permutations ~compare:Int.compare in
    Sequence.nth_exn permutations 999999
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:""
    |> printf "%s\n"
  ;;
end

include Solution.Make (M)
