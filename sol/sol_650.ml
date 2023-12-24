open! Core
open! Import

(* TODO: Make an int-as-prime-factors-representation arithmetic library. *)

let multiply_factors a b =
  Map.merge a b ~f:(fun ~key:_ ->
      function
      | `Both (x, y) -> Some (x + y)
      | `Left x -> Some x
      | `Right x -> Some x)
;;

let divide_factors a b =
  Map.merge a b ~f:(fun ~key:_ ->
      function
      | `Both (x, y) ->
        if x = y
        then None
        else (
          assert (x > y);
          Some (x - y))
      | `Left x -> Some x
      | `Right _ -> assert false)
;;

let pow_factors base expt = Map.map base ~f:(fun e -> e * expt)

module Infix = struct
  let ( *% ) = multiply_factors
  let ( /% ) = divide_factors
  let ( **% ) = pow_factors
end

open Infix

let factors_as_map n = Number_theory.Int.prime_factor n |> Map.of_alist_exn (module Int)

let rec factors_of_factorial =
  let cache = Hashtbl.create (module Int) in
  fun n ->
    Hashtbl.findi_or_add cache n ~default:(fun n ->
      if n = 0
      then Map.empty (module Int)
      else factors_of_factorial (n - 1) *% factors_as_map n)
;;

(* Corresponds to B(n)*)
let factors_of_product_of_binomials n =
  (factors_of_factorial n **% (n + 1))
  /% ((Sequence.range 0 n ~stop:`inclusive
       |> Sequence.map ~f:factors_of_factorial
       |> Sequence.fold
            ~init:(Map.empty (module Int))
            ~f:(fun acc factors -> acc *% factors))
      **% 2)
;;

(* TODO: Move this module into the Euler library. *)
module Int_modulo_1e9_7 : sig
  type t = int

  include Container.Summable with type t := t

  val ( * ) : t -> t -> t
  val pow : t -> int -> t
end = struct
  type t = int

  let modulus = 1_000_000_007
  let zero = 0
  let ( + ) x y = (x + y) mod modulus
  let ( * ) x y = x * y mod modulus

  let rec pow base expt =
    if expt = 0
    then 1
    else if expt mod 2 = 0
    then pow (base * base) (expt / 2)
    else base * pow base (expt - 1)
  ;;
end

module Int_maybe_modulo = Int_modulo_1e9_7

let rec sum_of_powers =
  let cache =
    Hashtbl.create
      (module struct
        type t = int * int [@@deriving compare, hash, sexp_of]
      end)
  in
  fun ~prime ~max_exponent ->
    Hashtbl.findi_or_add
      cache
      (prime, max_exponent)
      ~default:(fun (prime, max_exponent) ->
        let open Int_maybe_modulo in
        if max_exponent = 0
        then 1
        else pow prime max_exponent + sum_of_powers ~prime ~max_exponent:(max_exponent - 1))
;;

let sum_of_divisors_of_factors factors =
  Map.fold factors ~init:1 ~f:(fun ~key:prime ~data:exponent acc ->
    let open Int_maybe_modulo in
    acc * sum_of_powers ~prime ~max_exponent:exponent)
;;

(* Corresponds to D(n) *)
let sum_of_divisors_of_product_of_binomials n =
  n |> factors_of_product_of_binomials |> sum_of_divisors_of_factors
;;

let s n =
  Sequence.range 1 n ~stop:`inclusive
  |> Sequence.sum (module Int_maybe_modulo) ~f:sum_of_divisors_of_product_of_binomials
;;

let main () = s 20_000 |> printf "%d\n"

(* 538319652
   12h50m34.23025941s *)

include (val Solution.make ~problem:(Number 650) ~main)
