(** I like this interface better than [Core_kernel.Memo], and I have no need for
    the LRU stuff. *)

open! Core
open! Import

type ('a, 'b) fn = 'a -> 'b

val simple : (module Base.Hashtbl.Key with type t = 'a) -> ('a -> 'b) -> ('a, 'b) fn

val recursive
  :  (module Base.Hashtbl.Key with type t = 'a)
  -> (('a -> 'b) -> 'a -> 'b)
  -> ('a, 'b) fn
