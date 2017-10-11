open! Core

module type Prob = sig
  type t
  include Numerics_intf.Real with type t := t
  include Sexpable.S         with type t := t
end

module type S = sig
  type 'key t [@@deriving compare, sexp]
  type prob

  include Monad.S with type 'a t := 'a t

  val singleton : 'k -> 'k t

  val scale : 'k t -> prob -> 'k t

  val combine
    :  d1:'k t
    -> d2:'k t
    -> p1:prob
    -> 'k t

  val uniform  : 'k t list -> 'k t
  val uniform' : 'k   list -> 'k t

  val find     : 'k t -> 'k -> prob option
  val find_exn : 'k t -> 'k -> prob

  val of_map : ('k, prob) Map.Poly.t -> 'k t
  val to_map : 'k t -> ('k, prob) Map.Poly.t
end
