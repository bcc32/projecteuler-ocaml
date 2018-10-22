open! Core
open! Import

module M = struct
  let problem = Number 43

  let check_digits d =
    let is_div d1 d2 d3 p = ((100 * d1) + (10 * d2) + d3) mod p = 0 in
    match d with
    | [| _d1; d2; d3; d4; d5; d6; d7; d8; d9; d10 |] ->
      is_div d2 d3 d4 2
      && is_div d3 d4 d5 3
      && is_div d4 d5 d6 5
      && is_div d5 d6 d7 7
      && is_div d6 d7 d8 11
      && is_div d7 d8 d9 13
      && is_div d8 d9 d10 17
    | _ -> invalid_arg "wrong number of digits"
  ;;

  let main () =
    let digits = List.range 0 10 |> Array.of_list in
    let sum = ref 0 in
    let rec loop () =
      if check_digits digits
      then sum := !sum + Number_theory.Int.int_of_digits (Array.to_sequence_mutable digits);
      if Sequences.next_permutation_inplace digits ~compare:Int.compare then loop ()
    in
    loop ();
    printf "%d\n" !sum
  ;;

  (* 16695334890
     694.092ms *)
end

include Solution.Make (M)
