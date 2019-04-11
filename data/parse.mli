open! Core
open! Import

val space_separated_grid : string -> conv:(string -> 'a) -> 'a array array
val csv_line : string -> f:(string -> 'a) -> 'a list
