open! Core
open! Import

type ('a, 'b) fn = 'a -> 'b

let[@inline always] simple key f =
  let cache = Hashtbl.create key in
  fun x -> Hashtbl.findi_or_add cache x ~default:f
;;

let[@inline always] recursive key f =
  let cache = Hashtbl.create key in
  let rec memoized_f x = Hashtbl.findi_or_add cache x ~default:(f memoized_f) in
  memoized_f
;;
