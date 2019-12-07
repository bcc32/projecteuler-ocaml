open! Core
open! Import

let max_elt_plus_one = 62

type t = int [@@deriving compare, equal, hash]

let empty = 0
let mem t int = t land (1 lsl int) <> 0
let add t int = t lor (1 lsl int)
let remove t int = t land lnot (1 lsl int)
let union t u = t lor u
let inter t u = t land u
let diff t u = t land lnot u
let of_list ints = ints |> List.fold ~init:empty ~f:add

let fold t ~init ~f =
  Sequence.range 0 max_elt_plus_one
  |> Sequence.filter ~f:(mem t)
  |> Sequence.fold ~init ~f
;;

let iter t ~f =
  Sequence.range 0 max_elt_plus_one |> Sequence.filter ~f:(mem t) |> Sequence.iter ~f
;;

let length t = Int.popcount t

module C = Container.Make0 (struct
    type nonrec t = t

    module Elt = Int

    let fold = fold
    let iter = `Custom iter
    let length = `Custom length
  end)

let is_empty = C.is_empty
let fold_result = C.fold_result
let fold_until = C.fold_until
let exists = C.exists
let for_all = C.for_all
let count = C.count
let sum = C.sum
let find = C.find
let find_map = C.find_map
let to_list = C.to_list
let to_array = C.to_array
let min_elt = C.min_elt
let max_elt = C.max_elt
let sexp_of_t t = t |> to_list |> [%sexp_of: int list]
let t_of_sexp sexp = sexp |> [%of_sexp: int list] |> of_list
