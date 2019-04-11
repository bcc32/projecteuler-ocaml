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
  Re.all_seq word string
  |> Seq.map (fun groups -> Re.Group.get groups 1)
  |> Caml.List.of_seq
;;
