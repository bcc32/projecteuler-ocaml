(** Solutions to Project Euler problems, with conveniences for timing and
    command-line parsing. *)

open! Core
open! Import
module Make (M : Solution_intf.Arg) : Solution_intf.S
