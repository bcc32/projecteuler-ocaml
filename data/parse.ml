open! Core
open! Import

let space_separated_grid string ~conv =
  string
  |> String.split_lines
  |> Array.of_list_map ~f:(fun line ->
    String.split line ~on:' ' |> Array.of_list_map ~f:conv)
;;

let comma_separated_integers line =
  String.split line ~on:',' |> List.map ~f:Int.of_string
;;

let comma_separated_quoted_words string =
  let word =
    Re.compile
      (Re.seq [ Re.char '"'; Re.group (Re.rep (Re.compl [ Re.char '"' ])); Re.char '"' ])
  in
  let next_match = Re.all_gen word string in
  let rec loop words =
    match next_match () with
    | None -> List.rev words
    | Some groups -> loop (Re.Group.get groups 1 :: words)
  in
  loop []
;;
