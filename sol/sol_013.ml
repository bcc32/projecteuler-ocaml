open! Core
open! Import

module M = struct
  let problem = Number 13
  let prefix_length = 10

  let main () =
    Problem_013.data
    |> String.split_lines
    |> List.sum (module Bigint) ~f:Bigint.of_string
    |> Bigint.to_string
    |> String.subo ~len:prefix_length
    |> printf "%s\n"
  ;;

  (* 5537376230
     0.11ms *)
end

include Solution.Make (M)
