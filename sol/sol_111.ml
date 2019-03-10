open! Core
open! Import

let change_some_digits digits ~num_digits_to_change ~f =
  let rec loop ~pos ~num_digits_to_change =
    if num_digits_to_change = 0
    then f digits
    else if pos < Array.length digits
    then (
      loop ~pos:(pos + 1) ~num_digits_to_change;
      let old_digit = digits.(pos) in
      for new_digit = 0 to 9 do
        if old_digit <> new_digit
        then (
          digits.(pos) <- new_digit;
          loop ~pos:(pos + 1) ~num_digits_to_change:(num_digits_to_change - 1);
          digits.(pos) <- old_digit)
      done)
  in
  loop ~pos:0 ~num_digits_to_change
;;

let primes_with_runs ~main_digit ~num_digits =
  let digits = Array.create ~len:num_digits main_digit in
  with_return (fun { return } ->
    for num_digits_to_change = 0 to num_digits - 1 do
      let sum = ref 0 in
      change_some_digits digits ~num_digits_to_change ~f:(fun digits ->
        if digits.(0) <> 0
        then (
          let n =
            Number_theory.Int.int_of_digits (Array.to_sequence_mutable digits)
          in
          if Number_theory.Int.is_prime n then sum := !sum + n));
      if !sum <> 0 then return !sum
    done;
    assert false)
;;

module M = struct
  let problem = Number 111
  let num_digits = 10

  let main () =
    let sum = ref 0 in
    for main_digit = 0 to 9 do
      sum := !sum + (primes_with_runs ~main_digit ~num_digits : int)
    done;
    printf "%d\n" !sum
  ;;

  (* 66.153ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 612407567715 |}]
  ;;
end

include Solution.Make (M)
