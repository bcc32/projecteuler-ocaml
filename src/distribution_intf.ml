open! Core

module type Prob = Numerics_intf.Real

module type S = sig
  type 'key t
  type prob

  include Monad.S with type 'a t := 'a t

  val singleton : 'k -> 'k t

  val scale : 'k t -> prob -> 'k t

  val combine
    :  d1:'k t
    -> d2:'k t
    -> p1:prob
    -> 'k t

  val find     : 'k t -> 'k -> prob option
  val find_exn : 'k t -> 'k -> prob
end
