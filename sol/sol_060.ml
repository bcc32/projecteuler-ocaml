open! Core
open! Import

let limit = 100_000_000

(* log10(limit) *)
let limit_digits = 8
let is_prime = lazy (Number_theory.prime_sieve limit)

let primes : (int, immutable) Array.Permissioned.t Lazy.t =
  lazy
    (force is_prime
     |> Array.filter_mapi ~f:(fun i p -> Option.some_if p i)
     |> Array.Permissioned.of_array_id
     |> Array.Permissioned.copy)
;;

module Potential_prime_pair : sig
  type t = private
    { a : int
    ; b : int
    }

  val create : int -> int -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end = struct
  module T = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving compare, hash, sexp]

    let create a b = if a < b then { a; b } else { a = b; b = a }
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let is_prime_pair =
  let cache = Potential_prime_pair.Table.create () in
  fun a b ->
    let t = Potential_prime_pair.create a b in
    Hashtbl.findi_or_add cache t ~default:(fun { a; b } ->
      let concat a b =
        Sequence.append
          (Sequence.of_list (Number_theory.Int.digits_of_int a))
          (Sequence.of_list (Number_theory.Int.digits_of_int b))
        |> Number_theory.Int.int_of_digits
      in
      let c, d = concat a b, concat b a in
      let is_prime = force is_prime in
      c < Array.length is_prime
      && is_prime.(c)
      && d < Array.length is_prime
      && is_prime.(d))
;;

let count_digits = Number_theory.Int.fold_digits ~init:0 ~f:(fun ac _ -> ac + 1)

let add_one prime_pair_set ~target_cardinality =
  let hd = List.hd_exn prime_pair_set in
  let limit =
    let max_digits =
      if List.length prime_pair_set = target_cardinality - 1
      then limit_digits - count_digits hd
      else limit_digits / 2
    in
    Int.pow 10 max_digits - 1
  in
  if debug
  then Debug.eprint_s [%message "add_one" (prime_pair_set : int list) (limit : int)];
  force primes
  |> Array.Permissioned.to_sequence_immutable
  |> Sequence.drop_while ~f:(fun x -> x <= hd)
  |> Sequence.take_while ~f:(fun x -> x <= limit)
  |> Sequence.filter_map ~f:(fun prime ->
    if List.for_all (List.rev prime_pair_set) ~f:(fun x -> is_prime_pair x prime)
    then Some (prime :: prime_pair_set)
    else None)
;;

let rec find_prime_pair_sets orig_length length =
  if length = 1
  then
    force primes
    |> Array.Permissioned.to_sequence_immutable
    |> Sequence.filter ~f:(fun x -> 2 * count_digits x <= limit_digits)
    |> Sequence.map ~f:List.return
  else (
    let prev = find_prime_pair_sets orig_length (length - 1) in
    prev |> Sequence.concat_map ~f:(add_one ~target_cardinality:orig_length))
;;

module M = struct
  let problem = Number 60
  let length = 5

  let main () =
    if debug then Debug.eprintf !"%{Time} starting" (Time.now ());
    ignore (force primes : (int, _) Array.Permissioned.t);
    if debug then Debug.eprintf !"%{Time} done sieving" (Time.now ());
    find_prime_pair_sets length length
    |> Sequence.map ~f:(fun x ->
      if debug then Debug.eprint_s [%sexp (x : int list)];
      List.sum (module Int) x ~f:Fn.id)
    |> Sequence.min_elt ~compare:Int.compare
    |> uw
    |> printf "%d\n"
  ;;

  (* 26033
     19.5716s *)
end

include Solution.Make (M)