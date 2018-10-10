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
include Solution_intf.Export

let debug =
  match Sys.getenv "EULER_DEBUG" with
  | None
  | Some "" -> false
  | Some _ -> true
;;
