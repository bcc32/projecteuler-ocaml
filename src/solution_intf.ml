open! Core

module Solution_id = struct
  type t =
    [ `Number of int
    | `Custom of int * [`Key of string] * [`Description of string] ]
end

module type Arg = sig
  val problem : Solution_id.t
  val main : unit -> unit
end

module type S = sig
  val command : Command.t
  val command_name : string
end
