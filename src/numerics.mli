open! Core

module Make (M : Numerics_intf.Real) : Numerics_intf.S with type real = M.t

module Float : Numerics_intf.S with type real = float
module Bignum : Numerics_intf.S with type real = Bignum.Std.Bignum.t