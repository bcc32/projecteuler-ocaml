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
  val command : unit Term.t * Term.info
end
