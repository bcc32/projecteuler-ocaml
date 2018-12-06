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
      let digits = Number_theory.Int.digits_of_int pattern ~base:11 in
      Option.some_if (List.mem digits 10 ~equal:Int.equal) digits)
  ;;

  let replace_wildcards t ~fill_digit =
    t
    |> List.map ~f:(function
      | 10 -> fill_digit
      | n -> n)
    |> Sequence.of_list
    |> Number_theory.Int.int_of_digits
  ;;

  let instances t =
    match t with
    | 10 :: _ ->
      (* avoid leading zero *)
      List.range 1 10 |> List.map ~f:(fun fill_digit -> replace_wildcards t ~fill_digit)
    | t -> List.init 10 ~f:(fun fill_digit -> replace_wildcards t ~fill_digit)
  ;;
end

module M = struct
  let problem = Number 51
  let threshold = 8
  let prime_sieve = ref (1, [| false; false |])

  let rec is_prime n =
    if n > fst !prime_sieve
    then (
      let old_size = fst !prime_sieve in
      prime_sieve := old_size * 10, Number_theory.prime_sieve (old_size * 10);
      is_prime n)
    else (snd !prime_sieve).(n)
  ;;

  let main () =
    Pattern.all
    |> Sequence.find_map ~f:(fun pattern ->
      if debug then Debug.eprint_s [%sexp (pattern : Pattern.t)];
      let instances = Pattern.instances pattern in
      if List.count instances ~f:is_prime >= threshold
      then Some (List.hd_exn instances)
      else None)
    |> uw
    |> printf "%d\n"
  ;;

  (* 121313
     1.29861s *)
end

include Solution.Make (M)
