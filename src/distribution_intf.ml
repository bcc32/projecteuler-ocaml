open! Core
open! Import

(** Numbers that can represent probabilities *)
module type Prob = sig
  type t [@@deriving compare, quickcheck, sexp_of]

  val zero : t
  val of_int : int -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end

(** Monad operations that also require the comparator of the resulting support
    element type to be passed as an argument.  [map] and [bind] are called
    [map'] and [bind'], to avoid confusing naming with [Let_syntax]
    functions. *)
module type Monad2_with_explicit_key = sig
  type ('key, 'cmp) t
  type ('key, 'cmp) key = ('key, 'cmp) Comparator.Module.t

  val return : ('k, 'c) key -> 'k -> ('k, 'c) t

  (** [map' cmp t ~f] returns a new distribution with each element [x] of [t]'s
      support mapping to [f x].  The probabilities of elements that map to the
      same thing are combined. *)
  val map' : ('k2, 'c2) key -> ('k1, 'c1) t -> f:('k1 -> 'k2) -> ('k2, 'c2) t

  (** [bind' cmp t ~f] returns a new distribution with each element [x] of [t]'s
      support mapping to a new distribution, [f x].  The resulting distributions
      are added together. *)
  val bind' : ('k2, 'c2) key -> ('k1, 'c1) t -> f:('k1 -> ('k2, 'c2) t) -> ('k2, 'c2) t

  (*_ TODO: Implement map2':
    {[
      val map2'
        :  ('k3, 'c3) key
        -> ('k1, 'c1) t
        -> ('k2, 'c2) t
        -> f:('k1 -> 'k2 -> 'k3)
        -> ('k3, 'c3) t
    ]}
  *)
end

module type Infix2_with_fixed_key = sig
  type ('key, 'cmp) t

  val ( >>| ) : ('k, 'c) t -> ('k -> 'k) -> ('k, 'c) t
  val ( >>= ) : ('k, 'c) t -> ('k -> ('k, 'c) t) -> ('k, 'c) t
end

(** Monomorphic monad operations.

    Since [Base.Map] requires a comparator for the keys in order to construct a
    new map, the normal monad operators can't be as general as they should.
    But, it could still be useful for ergonomic reasons to have monomorphic
    versions. *)
module type Monad2_with_fixed_key = sig
  type ('key, 'cmp) t
  type ('key, 'cmp) key = ('key, 'cmp) Comparator.Module.t

  val return : ('k, 'c) key -> 'k -> ('k, 'c) t

  (** [map cmp t ~f] returns a new distribution with each element [x] of [t]'s
      support mapping to [f x].  The probabilities of elements that map to the
      same thing are combined. *)
  val map : ('k, 'c) t -> f:('k -> 'k) -> ('k, 'c) t

  (** [bind t ~f] returns a new distribution with each element [x] of [t]'s
      support mapping to a new distribution, [f x].  The resulting distributions
      are added together. *)
  val bind : ('k, 'c) t -> f:('k -> ('k, 'c) t) -> ('k, 'c) t

  (** [map2 t1 t2 ~f] returns a new distribution whose elements are [f x1 x2],
      for each pair of an element [x1] of [t1]'s support and an element [x2] of
      [t2]'s support. *)
  val map2 : ('k, 'c) t -> ('k, 'c) t -> f:('k -> 'k -> 'k) -> ('k, 'c) t

  val both
    :  ('k1, 'c1) t
    -> ('k2, 'c2) t
    -> ('k1 * 'k2, ('c1, 'c2) Tuple.T2.comparator_witness) t

  include Infix2_with_fixed_key with type ('key, 'cmp) t := ('key, 'cmp) t

  module Let_syntax : sig
    val return : ('k, 'c) key -> 'k -> ('k, 'c) t

    include Infix2_with_fixed_key with type ('key, 'cmp) t := ('key, 'cmp) t

    module Let_syntax : sig
      val bind : ('k, 'c) t -> f:('k -> ('k, 'c) t) -> ('k, 'c) t
      val map : ('k, 'c) t -> f:('k -> 'k) -> ('k, 'c) t

      val both
        :  ('k1, 'c1) t
        -> ('k2, 'c2) t
        -> ('k1 * 'k2, ('c1, 'c2) Tuple.T2.comparator_witness) t

      module Open_on_rhs : sig end
    end
  end
end

(** Module type representing a quickcheckable support element, required for
    deriving quickcheck on [Distribution]. *)
module type Quickcheck_m = sig
  type t [@@deriving quickcheck, sexp_of]

  include Comparator.S with type t := t
end

(** This module exists only to test that [(module Quickcheck_m)]
    satisfies [Comparator.Module.t]. *)
module Test_key_with_quickcheck_contains_comparator = struct
  module K : Quickcheck_m = Int

  let (_ : (_, _) Comparator.Module.t) = (module K)
end

(** Discrete probability distributions *)
module type S = sig
  (** a discrete probability distribution over elements (outcomes) of type 'key *)
  type ('key, 'cmp) t

  val sexp_of_t : ('k -> Sexp.t) -> ('k, 'c) t -> Sexp.t

  type ('key, 'cmp) key = ('key, 'cmp) Comparator.Module.t

  module Prob : Prob

  val quickcheck_generator
    :  ('k, 'c) key
    -> 'k Quickcheck.Generator.t
    -> ('k, 'c) t Quickcheck.Generator.t

  val quickcheck_observer : 'k Quickcheck.Observer.t -> ('k, 'c) t Quickcheck.Observer.t
  val quickcheck_shrinker : 'k Quickcheck.Shrinker.t -> ('k, 'c) t Quickcheck.Shrinker.t
  val compare : ('k, 'c) t -> ('k, 'c) t -> int

  include
    Monad2_with_fixed_key
      with type ('key, 'cmp) t := ('key, 'cmp) t
      with type ('key, 'cmp) key := ('key, 'cmp) key

  include
    Monad2_with_explicit_key
      with type ('key, 'cmp) t := ('key, 'cmp) t
      with type ('key, 'cmp) key := ('key, 'cmp) key

  (** [support t] returns a list of the possible outcomes described by the
      distribution [t] *)
  val support : ('k, 'c) t -> 'k list

  (** [singleton k] returns a distribution whose support consists of a single
      element, [k], with probability 1. *)
  val singleton : ('k, 'c) key -> 'k -> ('k, 'c) t

  (** {1 Accessors} *)

  (** usually 1 *)
  val total : ('k, 'c) t -> Prob.t

  (** [find t k] returns the probability of [k], or [None] if [k] is not in
      [support t]. *)
  val find : ('k, 'c) t -> 'k -> Prob.t option

  val find_exn : ('k, 'c) t -> 'k -> Prob.t
  val find_or_zero : ('k, 'c) t -> 'k -> Prob.t

  (** {1 Basic operations} *)

  (** [scale t x] multiplies the probability of each element of [t] by [x]. *)
  val scale : ('k, 'c) t -> Prob.t -> ('k, 'c) t

  (** [normalize t] scales [t] so that its total probability is 1. *)
  val normalize : ('k, 'c) t -> ('k, 'c) t

  (** {1 Combinators} *)

  (** [combine ~d1 ~d2 ~p1] combines two distributions into a single
      distribution, by selecting elements from [d1] with probability [p1] and
      elements from [d2] with probability [1 - p1]. *)
  val combine : d1:('k, 'c) t -> d2:('k, 'c) t -> p1:Prob.t -> ('k, 'c) t

  (** Same as [combine], but distinguishes between elements from different
      distributions. *)
  val combine'
    :  d1:('k1, 'c1) t
    -> d2:('k2, 'c2) t
    -> p1:Prob.t
    -> (('k1, 'k2) Either.t, ('c1, 'c2) Either.comparator_witness) t

  (** [uniform ts] returns a distribution that selects uniformly from the
      distributions in [ts]. *)
  val uniform : ('k, 'c) t list -> ('k, 'c) t

  (** [uniform'] is equivalent to [List.map ~f:singleton >> uniform] and is
      provided for convenience. *)
  val uniform' : ('k, 'c) key -> 'k list -> ('k, 'c) t

  (** [cartesian_product a b] is a distribution that selects one event from [a]
      and one from [b], independently. *)
  val cartesian_product
    :  ('k1, 'c1) t
    -> ('k2, 'c2) t
    -> ('k1 * 'k2, ('c1, 'c2) Tuple.T2.comparator_witness) t

  (** {1 Conversion functions}*)

  val of_map : ('k, Prob.t, 'c) Map.t -> ('k, 'c) t
  val to_map : ('k, 'c) t -> ('k, Prob.t, 'c) Map.t

  (** [of_alist alist] allows duplicate keys in [alist], adding their
      probabilities. *)
  val of_alist : ('k, 'c) key -> ('k, Prob.t) List.Assoc.t -> ('k, 'c) t

  (** [of_alist_exn alist] raises there are any duplicate keys in [alist]. *)
  val of_alist_exn : ('k, 'c) key -> ('k, Prob.t) List.Assoc.t -> ('k, 'c) t

  val to_alist
    :  ?key_order:[ `Decreasing | `Increasing ]
    -> ('k, 'c) t
    -> ('k, Prob.t) List.Assoc.t

  module M (K : sig
      type t
      type comparator_witness
    end) : sig
    type nonrec t = (K.t, K.comparator_witness) t
  end

  val sexp_of_m__t : (module Map.Sexp_of_m with type t = 'k) -> ('k, 'c) t -> Sexp.t
  val compare_m__t : (module Map.Compare_m) -> ('k, 'c) t -> ('k, 'c) t -> int

  val quickcheck_generator_m__t
    :  (module Quickcheck_m with type t = 'k and type comparator_witness = 'c)
    -> ('k, 'c) t Quickcheck.Generator.t

  val quickcheck_observer_m__t
    :  (module Quickcheck_m with type t = 'k and type comparator_witness = 'c)
    -> ('k, 'c) t Quickcheck.Observer.t

  val quickcheck_shrinker_m__t
    :  (module Quickcheck_m with type t = 'k and type comparator_witness = 'c)
    -> ('k, 'c) t Quickcheck.Shrinker.t
end

module type Distribution = sig
  (** A monadic representation of discrete probability distributions. *)

  module type Quickcheck_m = Quickcheck_m
  module type Prob = Prob
  module type S = S

  module Make (Prob : Prob) : S with module Prob = Prob
  module Float : S with type Prob.t = float
  module Percent : S with type Prob.t = Percent.t
  module Bignum : S with type Prob.t = Bignum.t
end
