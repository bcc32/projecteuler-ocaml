(** A monadic representation of discrete probability distributions. *)

open! Core
open! Import

module type Prob = Distribution_intf.Prob
module type S = Distribution_intf.S

module Make (Prob : Prob) : S with module Prob = Prob
module Float : S with type Prob.t = float
module Percent : S with type Prob.t = Percent.t
module Bignum : S with type Prob.t = Bignum.t
