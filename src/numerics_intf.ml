open! Core
open! Import

module type Real = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val of_int : int -> t
  val abs : t -> t
  val sign_exn : t -> Sign.t

  include Comparable.S with type t := t
end

module type S = sig
  type real

  val bisect : f:(real -> real) -> epsilon:real -> low:real -> high:real -> real

  (** default is `Simpson's_rule *)
  val integrate
    :  ?method_:[`Midpoint | `Trapezoid | `Simpson's_rule]
    -> unit
    -> f:(real -> real)
    -> low:real
    -> high:real
    -> intervals:int
    -> real

  val newton's_method
    :  f:(real -> real)
    -> f':(real -> real)
    -> epsilon:real
    -> init:real
    -> real
end
