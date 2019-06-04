open! Core
open! Import

module Mask : sig
  (** A bitmask over digits.  [1] means that digit's position can be filled by
      another digit. *)
  type t = private int

  val of_bits : int -> t
  val iter_all : digits:int -> f:(t -> unit) -> unit
end = struct
  type t = int

  let of_bits = Fn.id

  let iter_all ~digits ~f =
    for t = 0 to (1 lsl digits) - 1 do
      f t
    done
  ;;
end

module Masked_number : sig
  (** Masked numbers are ints in base-11 where digit A represents a masked
      digit. *)
  type t = private int [@@deriving compare, hash, sexp_of]

  (** [of_int n ~mask] checks to make sure that all the would-be masked digits
      in [n] are the same, and if so, returns the masked [n]. *)
  val of_int : int -> mask:Mask.t -> t option

  val iter_all : int -> digits:int -> f:(t -> unit) -> unit
  val fill : t -> int list
end = struct
  type t = int [@@deriving compare, hash]

  module Base11 = Number_theory.Int.As_digits (struct
      let base = 11
    end)

  let sexp_of_t t =
    Base11.to_list t
    |> List.rev_map ~f:(function
      | 10 -> "?"
      | n -> Int.to_string n)
    |> String.concat
    |> Sexp.Atom
  ;;

  let of_int n ~mask =
    let rec loop n ~mask ~acc ~prev_digit =
      if n = 0
      then Some acc
      else (
        let digit = n mod 10 in
        if mask land 1 <> 0
        then
          if prev_digit <> -1 && prev_digit <> digit
          then None
          else loop (n / 10) ~mask:(mask lsr 1) ~acc:((11 * acc) + 10) ~prev_digit:digit
        else loop (n / 10) ~mask:(mask lsr 1) ~acc:((11 * acc) + digit) ~prev_digit)
    in
    loop n ~mask:(mask : Mask.t :> int) ~acc:0 ~prev_digit:(-1)
  ;;

  let iter_all n ~digits ~f =
    Mask.iter_all ~digits ~f:(fun mask -> Option.iter (of_int n ~mask) ~f)
  ;;

  let fill t =
    let fill_with_digit t digit =
      Base11.Right_to_left.fold t ~init:0 ~f:(fun acc d ->
        match d with
        | 10 -> (10 * acc) + digit
        | n -> (10 * acc) + n)
    in
    List.init 10 ~f:(fill_with_digit t)
  ;;
end

let%expect_test "Masked_number.of_int" =
  Masked_number.of_int 121313 ~mask:(Mask.of_bits 0b000101)
  |> [%sexp_of: Masked_number.t option]
  |> print_s;
  [%expect {| (121?1?) |}];
  Masked_number.of_int 121313 ~mask:(Mask.of_bits 0b011010)
  |> [%sexp_of: Masked_number.t option]
  |> print_s;
  [%expect {| () |}]
;;

let%expect_test "Masked_number.iter_all" =
  Masked_number.iter_all 121313 ~digits:6 ~f:(printf !"%{sexp: Masked_number.t}\n");
  [%expect
    {|
    121313
    12131?
    1213?3
    121?13
    121?1?
    12?313
    12?3?3
    1?1313
    ?21313
    ?213?3
    ?2?313
    ?2?3?3 |}]
;;

let problem = Number 51
let threshold = 8

let main () =
  let rec loop digits =
    let mask_counts = Hashtbl.create (module Masked_number) in
    let is_prime = Number_theory.prime_sieve (Int.pow 10 digits) in
    for p = Int.pow 10 (digits - 1) to Int.pow 10 digits - 1 do
      if is_prime.(p)
      then
        Masked_number.iter_all p ~digits ~f:(fun masked_int ->
          Hashtbl.incr mask_counts masked_int)
    done;
    match
      with_return_option (fun { return } ->
        Hashtbl.iteri mask_counts ~f:(fun ~key:mask ~data:count ->
          if count >= threshold
          then (
            let primes =
              Masked_number.fill mask |> List.filter ~f:(fun n -> is_prime.(n))
            in
            if debug
            then (
              Debug.eprint_s [%sexp (mask : Masked_number.t)];
              Debug.eprint_s [%sexp (primes : int list)]);
            return primes)))
    with
    | None -> loop (digits + 1)
    | Some primes ->
      let lowest = List.min_elt primes ~compare:Int.compare |> Option.value_exn in
      printf "%d\n" lowest
  in
  loop 1
;;

(* 121313
   720.07ms *)

include (val Solution.make ~problem ~main)
