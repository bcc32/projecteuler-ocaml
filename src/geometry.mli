(** Geometric identities and formulas. *)

open! Core
open! Import
(* TODO Add law of sines, cosines, etc. *)
(* TODO Add formula for triangle area based on sides. *)
(* TODO Add formula for polygon area based on lattice points. *)

(** [is_pythagorean_triple a b c] is equivalent to [a^2 + b^2 = c^2]. *)
val is_pythagorean_triple : int -> int -> int -> bool
