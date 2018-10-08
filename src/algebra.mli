(** Basic algebra. *)

open! Core
open! Import

(** [quadratic_formula a b c] returns real-valued solutions to [a x^2 + b x + c
    = 0], for [a <> 0].  If there are two solutions, the first returned result is
    smaller than the second. *)
val quadratic_formula :
  float
  -> float
  -> float
  -> [`Two of float * float | `One of float | `None]
