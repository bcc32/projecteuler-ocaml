open! Core
open! Import

(** Numbers that can represent probabilities *)
module type Prob = sig
  type t [@@deriving compare, sexp]

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
  type 'key t [@@deriving compare, sexp]

  (** a numerical type capable of representing probabilities *)
  type prob

  include Monad.S with type 'a t := 'a t

  (** [support t] returns a list of the possible outcomes described by the
      distribution [t] *)
  val support : 'k t -> 'k list

  (** [singleton k] returns a distribution whose support consists of a single
      element, [k], with probability 1. *)
  val singleton : 'k -> 'k t

  (** {1 Accessors} *)

  (** usually 1 *)
  val total : 'k t -> prob

  (** [find t k] returns the probability of [k], or [None] if [k] is not in
      [support t]. *)
  val find : 'k t -> 'k -> prob option

  val find_exn : 'k t -> 'k -> prob
  val find_or_zero : 'k t -> 'k -> prob

  (** {1 Basic operations} *)

  val scale : 'k t -> prob -> 'k t
  val normalize : 'k t -> 'k t

  (** {1 Combinators} *)

  (** [combine ~d1 ~d2 ~p1] combines two distributions into a single
      distribution, by selecting elements from [d1] with probability [p1] and
      elements from [d2] with probability [1 - p1]. *)
  val combine : d1:'k t -> d2:'k t -> p1:prob -> 'k t

  (** Same as [combine], but distinguishes between events from the different
      distributions. *)
  val combine' : d1:'a t -> d2:'b t -> p1:prob -> ('a, 'b) Either.t t

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

  val of_map : ('k, prob) Map.Poly.t -> 'k t
  val to_map : 'k t -> ('k, prob) Map.Poly.t
  val of_alist_exn : ('k, prob) List.Assoc.t -> 'k t
  val to_alist : 'k t -> ('k, prob) List.Assoc.t
end
