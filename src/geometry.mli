(** Geometric identities and formulas. *)

open! Core
open! Import

(*_ TODO Add law of sines, cosines, etc. *)
(*_ TODO Add formula for triangle area based on sides. *)
(*_ TODO Add formula for polygon area based on lattice points. *)

(** [is_pythagorean_triple a b c] is equivalent to [a^2 + b^2 = c^2]. *)
val is_pythagorean_triple : int -> int -> int -> bool

val iter_all_pythagorean_triples
  :  with_hypotenuse_less_than:int
  -> f:(int -> int -> int -> unit)
  -> unit

(** [pythagorean_triples] *)
val pythagorean_triples : (int * int * int) Sequence.t
