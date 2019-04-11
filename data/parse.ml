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
  let reader = Csv_reader.Row.builder |> Csv_reader.map ~f:Csv_reader.Row.to_list in
  match string |> Csv_reader.list_of_string reader with
  | [] -> failwith "no rows"
  | [ row ] -> row
  | rows ->
    raise_s
      [%message
        "comma_separated_quoted_words: too many rows" ~_:(List.length rows : int)]
;;
