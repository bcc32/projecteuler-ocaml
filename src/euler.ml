open! Core
open! Import
module Solution = Solution
module Solution_intf = Solution_intf
module Algebra = Algebra
module Distribution = Distribution
module Geometry = Geometry
module Numerics = Numerics
module Number_theory = Number_theory
module Sequences = Sequences

type solution_id = Solution_intf.Solution_id.t =
  | Number of int
  | Custom of { number : int; tag : string; description : string }
