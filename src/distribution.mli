open! Core

module Make (Prob : Distribution_intf.Prob) : Distribution_intf.S
  with type prob = Prob.t

module Float : Distribution_intf.S with type prob = float

module Bignum : Distribution_intf.S with type prob = Bignum.t
