open! Core
open! Import

module M = struct
  let problem = Number 13
  let path = "data/013.txt"
  let prefix_length = 10

  let numbers =
    lazy
      (In_channel.with_file path ~f:(fun chan ->
         In_channel.input_lines chan |> List.map ~f:Bigint.of_string))
  ;;

  let main () =
    force numbers
    |> List.sum (module Bigint) ~f:Fn.id
    |> Bigint.to_string
    |> String.subo ~len:prefix_length
    |> printf "%s\n"
  ;;
end

include Solution.Make (M)
