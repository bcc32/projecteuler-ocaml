(** Solutions to Project Euler problems, with conveniences for timing and
    command-line parsing. *)

open! Core
open! Import

module type Arg = Solution_intf.Arg
module type S = Solution_intf.S

module Make (M : Arg) : S
