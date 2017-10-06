open! Core

module type Real = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

  val of_int : int -> t

  val sign : t -> Sign.t
  val abs  : t -> t

  include Comparable.S with type t := t
end

module type S = sig
  type real
  val bisect
    :  f : (real -> real)
    -> epsilon : real
    -> low     : real
    -> high    : real
    -> real
  val newton's_method
    :  f  : (real -> real)
    -> f' : (real -> real)
    -> epsilon : real
    -> init    : real
    -> real
end
