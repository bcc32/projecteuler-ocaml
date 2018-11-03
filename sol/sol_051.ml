open! Core
open! Import

let all_same n mask =
  let rec loop n prev mask =
    if n = 0
    then true
    else (
      let digit = n mod 10 in
      if mask land 1 <> 0
      then (prev = -1 || prev = digit) && loop (n / 10) digit (mask lsr 1)
      else loop (n / 10) prev (mask lsr 1))
  in
  loop n (-1) mask
;;

(* TODO make mask a type *)
(* masked numbers are ints in base-11 where digit A represents a masked digit *)
let sexp_of_mask mask =
  Number_theory.Int.to_digits mask ~base:11
  |> List.rev_map ~f:(function
    | 10 -> "?"
    | n -> Int.to_string n)
  |> String.concat
  |> Sexp.Atom
;;

let apply_mask n mask =
  let rec loop n mask acc =
    if n = 0
    then acc
    else (
      let digit = if mask land 1 <> 0 then 10 else n mod 10 in
      loop (n / 10) (mask lsr 1) ((11 * acc) + digit))
  in
  loop n mask 0
;;

let iter_masked n ~digits ~f =
  for mask = 0 to (1 lsl digits) - 1 do
    if all_same n mask then f (apply_mask n mask)
  done
;;

let%expect_test "iter_masked" =
  iter_masked 121313 ~digits:6 ~f:(printf !"%{sexp: mask}\n");
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

let fill_mask mask =
  let fill mask digit =
    Number_theory.Int.fold_digits mask ~base:11 ~init:0 ~f:(fun acc d ->
      match d with
      | 10 -> (10 * acc) + digit
      | n -> (10 * acc) + n)
  in
  List.init 10 ~f:(fill mask)
;;

module M = struct
  let problem = Number 51
  let threshold = 8

  let main () =
    let rec loop digits =
      let mask_counts = Int.Table.create () in
      let is_prime = Number_theory.prime_sieve (Int.pow 10 digits) in
      for p = Int.pow 10 (digits - 1) to Int.pow 10 digits - 1 do
        if is_prime.(p)
        then
          iter_masked p ~digits ~f:(fun masked_int -> Hashtbl.incr mask_counts masked_int)
      done;
      match
        with_return_option (fun { return } ->
          Hashtbl.iteri mask_counts ~f:(fun ~key:mask ~data:count ->
            if count >= threshold
            then (
              let primes =
                fill_mask mask |> List.filter ~f:(fun n -> is_prime.(n))
              in
              if debug
              then (
                Debug.eprint_s [%sexp (mask : mask)];
                Debug.eprint_s [%sexp (primes : int list)]);
              primes |> List.min_elt ~compare:Int.compare |> uw |> return)))
      with
      | None -> loop (digits + 1)
      | Some lowest -> printf "%d\n" lowest
    in
    loop 1
  ;;

  (* 121313
     720.07ms *)
end

include Solution.Make (M)
