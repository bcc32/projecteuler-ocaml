(** A monadic representation of discrete probability distributions. *)

open! Core
open! Import

module type Prob = Distribution_intf.Prob
module type S = Distribution_intf.S

module Make (Prob : Prob) : S with type prob = Prob.t
module Float : S with type prob = float
module Percent : S with type prob = Percent.t
module Bignum : S with type prob = Bignum.t
