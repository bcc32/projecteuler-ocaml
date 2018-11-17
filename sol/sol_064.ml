open! Core
open! Import

module Continued_fraction_stage : sig
  (** (a, b, c) represents (sqrt(a) + b) / c *)
  type t = int * int * int

  (** [pop t] returns [(x, t')] such that [t = x + 1 / t'] *)
  val pop : t -> int * t

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)

  let pop (a, b, c) =
    let floor_sqrt_a = Number_theory.Int.isqrt a in
    let x = (floor_sqrt_a + b) / c in
    (* 1 / (t - x)

       = 1 / ((sqrt(a) + b) / c - x)
       = c / (sqrt(a) + (b + x * c))

       let y = b + x * c, then:

       c / (sqrt(a) + y)

       = (c * (sqrt(a) - y)) / (a - y^2)
       = (sqrt(c^2 * a) - c * y) / (a - y^2)

       = TODO
    *)
    let t' =
      let y = b + x * c in
    in
    x, t'
  ;;
end

  let main () = ()
include (val Solution.make ~problem:(Number 64) ~main)
