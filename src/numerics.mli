(** Numerical integration, root-finding, etc. *)

open! Core
open! Import
module Make (M : Numerics_intf.Real) : Numerics_intf.S with type real = M.t
module Float : Numerics_intf.S with type real = float
module Bignum : Numerics_intf.S with type real = Bignum.t
