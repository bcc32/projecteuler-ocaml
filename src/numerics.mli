(** Numerical integration, root-finding, etc. *)

open! Core
open! Import

module type Real = Numerics_intf.Real
module type S = Numerics_intf.S

module Make (M : Real) : S with type real = M.t
module Float : S with type real = float
module Bignum : S with type real = Bignum.t
