open! Core

type solution_id =
  [ `Number of int
  | `Custom of int * [ `Key of string ] * [ `Description of string ] ]

module type Solution = sig
  val problem : solution_id
  val main : unit -> unit
end

module type S = sig
  val command : Command.t
  val command_name : string
end

module Make (M : Solution) : S
