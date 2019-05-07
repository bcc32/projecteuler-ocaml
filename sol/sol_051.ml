open! Core
open! Import

(* Consider each digit in a pattern as either 0-9 or a wildcard.  We can model
   this as base-11 numbers where digit A represents a wildcard. *)
module Pattern : sig
  type t [@@deriving sexp_of]

  val all : t Sequence.t
  val instances : t -> int list
end = struct
  type t = int list

  (* digits *)

  let sexp_of_t t =
    List.map t ~f:(function
      | 10 -> "?"
      | n -> Int.to_string n)
    |> String.concat
    |> Sexp.Atom
  ;;

  let all =
    Number_theory.Int.natural_numbers ()
    |> Sequence.filter_map ~f:(fun pattern ->
      let digits = Number_theory.Int.to_digits pattern ~base:11 in
      Option.some_if (List.mem digits 10 ~equal:Int.equal) digits)
  ;;

  let replace_wildcards t ~fill_digit =
    List.fold t ~init:0 ~f:(fun acc -> function
      | 10 -> (10 * acc) + fill_digit
      | n -> (10 * acc) + n)
  ;;

  let instances t =
    match t with
    | 10 :: _ ->
      (* avoid leading zero *)
      List.init 9 ~f:(fun fill_digit_minus_one ->
        replace_wildcards t ~fill_digit:(fill_digit_minus_one + 1))
    | t -> List.init 10 ~f:(fun fill_digit -> replace_wildcards t ~fill_digit)
  ;;
end

module Prime_sieve = struct
  type t =
    { mutable upper_limit : int
    ; mutable is_prime : bool array
    }

  let create () = { upper_limit = 1; is_prime = [| false; false |] }

  let rec is_prime t n =
    if n > t.upper_limit
    then (
      let old_size = t.upper_limit in
      let new_size = old_size * 10 in
      let new_sieve = Number_theory.prime_sieve new_size in
      t.upper_limit <- new_size;
      t.is_prime <- new_sieve;
      is_prime t n)
    else t.is_prime.(n)
  ;;
end

module M = struct
  let problem = Number 51
  let threshold = 8

  let main () =
    let sieve = Prime_sieve.create () in
    Pattern.all
    |> Sequence.find_map ~f:(fun pattern ->
      if debug then Debug.eprint_s [%sexp (pattern : Pattern.t)];
      let instances = Pattern.instances pattern in
      if List.count instances ~f:(Prime_sieve.is_prime sieve) >= threshold
      then Some (List.hd_exn instances)
      else None)
    |> uw
    |> printf "%d\n"
  ;;

  (* 121313
     1.29861s *)
end

include Solution.Make (M)
