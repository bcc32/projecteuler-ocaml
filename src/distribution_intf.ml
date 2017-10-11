open! Core

module type Prob = sig
  type t
  include Commutative_group.S with type t := t
  include Numerics_intf.Real  with type t := t
  include Sexpable.S          with type t := t
end

module type S = sig
  type 'key t [@@deriving compare, sexp]
  type prob

  include Monad.S with type 'a t := 'a t

  val singleton : 'k -> 'k t

  val scale : 'k t -> prob -> 'k t
  val normalize : 'k t -> 'k t

  val combine
    :  d1:'k t
    -> d2:'k t
    -> p1:prob
    -> 'k t

  val uniform  : 'k t list -> 'k t
  val uniform' : 'k   list -> 'k t

  val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

  (** usually 1 **)
  val total : 'k t -> prob

  val find     : 'k t -> 'k -> prob option
  val find_exn : 'k t -> 'k -> prob

  val of_map : ('k, prob) Map.Poly.t -> 'k t
  val to_map : 'k t -> ('k, prob) Map.Poly.t

  val of_alist_exn : ('k, prob) List.Assoc.t -> 'k t
  val to_alist     : 'k t -> ('k, prob) List.Assoc.t
end
