(** Solutions to Project Euler problems, with conveniences for timing and
    command-line parsing. *)

open! Core
open! Import

module Solution_id = struct
  type t =
    | Number of int
    | Tagged of { number : int; tag : string; description : string }
    | Custom of { name : string; description : string }
end

module Export = struct
  type _solution_id = Solution_id.t =
    | Number of int
    | Tagged of { number : int; tag : string; description : string }
    | Custom of { name : string; description : string }
end

module type Arg = sig
  val problem : Solution_id.t
  val main : unit -> unit
end

module type S = sig
  (** Name to identify this solution. *)
  val name : string

  (** One-line human-readable description of this solution. *)
  val description : string

  (** [run ?print_debug ?print_elapsed_time ()] this solution, maybe printing
      progress/debug output and at the end, the time elapsed.

      @param print_debug default = false
      @param print_elapsed_time default = false *)
  val run : ?print_debug:bool -> ?print_elapsed_time:bool -> unit -> unit
end

module type Solution = sig
  module type Arg = Arg
  module type S = S

  type t = ?print_debug:bool -> ?print_elapsed_time:bool -> unit -> unit

  module Make (M : Arg) : S
end
