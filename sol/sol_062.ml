open! Core
open! Import

module Digit_set : sig
  type t [@@deriving sexp]

  val of_int : int -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end = struct
  module T = struct
    type t = int list [@@deriving compare, hash, sexp]

    let of_int int = Number_theory.Int.to_digits int |> List.sort ~compare:Int.compare
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let find min_permutations =
  let cubes_by_digit_set = Digit_set.Table.create () in
  let[@inline] cube n = Number_theory.Int.addition_chain_pow n 3 in
  let rec loop n =
    let c = cube n in
    let ds = Digit_set.of_int c in
    Hashtbl.add_multi cubes_by_digit_set ~key:ds ~data:c;
    let cubes = Hashtbl.find_multi cubes_by_digit_set ds in
    if List.length cubes >= min_permutations
    then uw (List.min_elt cubes ~compare:Int.compare)
    else loop (n + 1)
  in
  loop 1
;;

module M = struct
  let problem = Number 62
  let min_permutations = 5
  let main () = find min_permutations |> printf "%d\n"

  (* 127035954683
     29.618ms *)
end

include Solution.Make (M)
