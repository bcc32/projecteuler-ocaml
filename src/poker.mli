(** Standard 52-card poker cards *)

open! Core
open! Import

module Rank : sig
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

  val pred : t -> t option
  val succ : t -> t option

  include Comparable.S with type t := t
  include Hashable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t
end

module Suit : sig
  type t =
    | Clubs
    | Diamonds
    | Hearts
    | Spades

  include Comparable.S with type t := t
  include Hashable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t
end

module Card : sig
  type t = private
    { rank : Rank.t
    ; suit : Suit.t
    }
  [@@deriving fields]

  (** 2C, QH, etc. *)
  include Stringable.S with type t := t

  (** compare by rank, then by suit *)
  include Comparable.S with type t := t

  include Hashable.S with type t := t
  include Sexpable.S with type t := t
end

module Hand_classification : sig
  (** Each [Rank.t] represents the highest card in each group, starting with the
      largest multiplicity (ties broken by rank, descending). *)
  type t =
    | High_card of Rank.t * Rank.t * Rank.t * Rank.t * Rank.t
    | One_pair of Rank.t * Rank.t * Rank.t * Rank.t
    | Two_pairs of Rank.t * Rank.t * Rank.t
    | Three_of_a_kind of Rank.t * Rank.t * Rank.t
    | Straight of Rank.t
    | Flush of Rank.t * Rank.t * Rank.t * Rank.t * Rank.t
    | Full_house of Rank.t * Rank.t
    | Four_of_a_kind of Rank.t * Rank.t
    | Straight_flush of Rank.t  (** [rank < Ace] *)
    | Royal_flush

  include Comparable.S with type t := t
  include Invariant.S with type t := t
  include Sexpable.S with type t := t
end

module Hand : sig
  type t

  (** raises unless length is 5 *)
  val of_card_list_exn : Card.t list -> t

  val classify : t -> Hand_classification.t
end
