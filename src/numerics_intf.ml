open! Core
open! Import

(** Real numbers *)
module type Real = sig
  type t [@@deriving sexp_of]

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val of_int : int -> t
  val abs : t -> t
  val sign_exn : t -> Sign.t

  include Comparable with type t := t
end

module type S = sig
  (** a numeric type capable of representing (possibly approximate) real numbers *)
  type real

  (* FIXME either rename to delta or make it a relative error *)

  (** [bisect ~f ~epsilon ~lo ~hi] finds a solution [x] to the equation [f x =
      0].

      @return a value in [[lo, hi]] no farther than [epsilon] from a solution to
      [f x = 0]. *)
  val bisect : f:(real -> real) -> epsilon:real -> lo:real -> hi:real -> real

  (** Integrate [f] numerically on the interval (lo, hi), approximating with
      the given number of [intervals].

      @param method_ default is [`Simpson's_rule] *)
  val integrate
    :  ?method_:[ `Midpoint | `Trapezoid | `Simpson's_rule ]
    -> unit
    -> f:(real -> real)
    -> lo:real
    -> hi:real
    -> intervals:int
    -> real

  (* FIXME consistent use of error in x or y *)

  (** [newton's_method ~f ~f' ~epsilon ~init] finds a solution [x] to the
      equation [f x = 0].

      @param f' the derivative of [f]

      @return a value no farther than [epsilon] from a solution to [f x = 0]. *)
  val newton's_method
    :  f:(real -> real)
    -> f':(real -> real)
    -> epsilon:real
    -> init:real
    -> real
end

module type Numerics = sig
  (** Numerical integration, root-finding, etc. *)

  module type Real = Real
  module type S = S

  module Make (M : Real) : S with type real = M.t
  module Float : S with type real = float
  module Bignum : S with type real = Bignum.t
end
