open! Core
open! Import

let space_separated_grid string ~conv =
  string
  |> String.split_lines
  |> Array.of_list_map ~f:(fun line ->
    String.split line ~on:' ' |> Array.of_list_map ~f:conv)
;;

let csv_line line ~f =
  let reader =
    let open Csv.Let_syntax in
    let%map row = Csv.Row.builder in
    List.init (Csv.Row.length row) ~f:(fun i -> Csv.Row.nth_conv_exn row i [%here] f)
  in
  match line |> Csv.list_of_string reader with
  | [] -> failwith "no rows"
  | [ row ] -> row
  | rows ->
    raise_s
      [%message
        "comma_separated_quoted_words: too many rows" ~_:(List.length rows : int)]
;;
