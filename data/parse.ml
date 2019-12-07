open! Core
open! Import

let split_line_on_spaces =
  let ws_rex =
    let open Re in
    compile (rep1 space)
  in
  Re.split ws_rex
;;

let space_separated_grid string ~conv =
  string
  |> String.split_lines
  |> Array.of_list_map ~f:(fun line ->
    line |> split_line_on_spaces |> Array.of_list_map ~f:conv)
;;

let csv_line line ~f =
  let reader =
    let open Csv.Let_syntax in
    Csv.Row.builder >>| Csv.Row.to_list >>| List.map ~f
  in
  match line |> Csv.list_of_string reader with
  | [] -> failwith "no rows"
  | [ row ] -> row
  | rows -> raise_s [%message "csv_line: too many rows" ~_:(List.length rows : int)]
;;
