open! Core
open! Import

(** Numbers that can represent probabilities *)
module type Prob = sig
  type t [@@deriving compare, quickcheck, sexp]

  val zero : t
  val of_int : int -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( // ) : int -> int -> t
end

(** Discrete probability distributions *)
module type S = sig
  (** a discrete probability distribution over elements (outcomes) of type 'key *)
  type 'key t [@@deriving compare, quickcheck, sexp]

  module Prob : Prob
  include Monad with type 'a t := 'a t

  (** [support t] returns a list of the possible outcomes described by the
      distribution [t] *)
  val support : 'k t -> 'k list

  (** [singleton k] returns a distribution whose support consists of a single
      element, [k], with probability 1. *)
  val singleton : 'k -> 'k t

  (** {1 Accessors} *)

  (** usually 1 *)
  val total : 'k t -> Prob.t

  (** [find t k] returns the probability of [k], or [None] if [k] is not in
      [support t]. *)
  val find : 'k t -> 'k -> Prob.t option

  val find_exn : 'k t -> 'k -> Prob.t
  val find_or_zero : 'k t -> 'k -> Prob.t

  (** {1 Basic operations} *)

  val scale : 'k t -> Prob.t -> 'k t
  val normalize : 'k t -> 'k t

  (** {1 Combinators} *)

  (** [combine ~d1 ~d2 ~p1] combines two distributions into a single
      distribution, by selecting elements from [d1] with probability [p1] and
      elements from [d2] with probability [1 - p1]. *)
  val combine : d1:'k t -> d2:'k t -> p1:Prob.t -> 'k t

  (** Same as [combine], but distinguishes between events from the different
      distributions. *)
  val combine' : d1:'a t -> d2:'b t -> p1:Prob.t -> ('a, 'b) Either.t t

  (** [uniform ts] returns a distribution that selects uniformly from the
      distributions in [ts]. *)
  val uniform : 'k t list -> 'k t

  (** [uniform'] is equivalent to [List.map ~f:singleton >> uniform] and is
      provided for convenience. *)
  val uniform' : 'k list -> 'k t

  (** [cartesian_product a b] is a distribution that selects one event from [a]
      and one from [b], independently. *)
  val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

  (** {1 Conversion functions}*)

  val of_map : ('k, Prob.t) Map.Poly.t -> 'k t
  val to_map : 'k t -> ('k, Prob.t) Map.Poly.t
  val of_alist_exn : ('k, Prob.t) List.Assoc.t -> 'k t
  val to_alist : 'k t -> ('k, Prob.t) List.Assoc.t
end

module type Distribution = sig
  (** A monadic representation of discrete probability distributions. *)

  module type Prob = Prob
  module type S = S

  module Make (Prob : Prob) : S with module Prob = Prob
  module Float : S with type Prob.t = float
  module Percent : S with type Prob.t = Percent.t
  module Bignum : S with type Prob.t = Bignum.t
end
