open! Core
open! Import

module M = struct
  let problem = Number 3
  let main () = Number_theory.Int.factor 600851475143 |> List.last_exn |> printf "%d\n"
end

include Solution.Make (M)
