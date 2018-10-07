open! Core
open! Import

module M = struct
  let problem = Number 13
  let prefix_length = 10

  let main () =
    Problem_013.data
    |> String.split_lines
    |> List.map ~f:Bigint.of_string
    |> List.sum (module Bigint) ~f:Fn.id
    |> Bigint.to_string
    |> String.subo ~len:prefix_length
    |> printf "%s\n"
  ;;
end

include Solution.Make (M)
