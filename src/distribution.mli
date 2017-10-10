open! Core

module Make (Prob : Distribution_intf.Prob) : Distribution_intf.S
  with type prob = Prob.t
