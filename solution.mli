open Core.Std

module type Solution = sig
  val problem_number : int
  val main : unit -> unit
end

module type S = sig
  val command : Command.t
end

module Make (M : Solution) : S
