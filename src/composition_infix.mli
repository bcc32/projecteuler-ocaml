(** Infix operators for function composition. *)

open! Core

module Export : sig
  val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

include module type of Export
