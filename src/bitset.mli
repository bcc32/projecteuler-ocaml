open! Core
open! Import

(** Represents a subset of \(\[0, 62)\). *)
type t [@@immediate] [@@deriving compare, equal, hash, sexp]

include Container.S0 with type t := t and type elt := int

val empty : t
val add : t -> int -> t
val remove : t -> int -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val of_list : int list -> t
