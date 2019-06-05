open! Core
open! Import

let is_div d1 d2 d3 p = ((100 * d1) + (10 * d2) + d3) mod p = 0

let follows ~requirements digit digits =
  match requirements.(digit) with
  | None -> true
  | Some modulus
    when is_div digits.(digit - 2) digits.(digit - 1) digits.(digit) modulus -> true
  | _ -> false
;;

let rec iter digit digits ~is_used ~requirements ~f =
  if digit >= Array.length requirements
  then f (digits |> Array.to_sequence_mutable |> Number_theory.Int.As_base10.of_sequence)
  else
    for d = 0 to 9 do
      digits.(digit) <- d;
      if not is_used.(d) && follows ~requirements digit digits
      then (
        is_used.(d) <- true;
        iter (digit + 1) digits ~is_used ~requirements ~f;
        is_used.(d) <- false)
    done
;;

let sum_pandigital () =
  let sum = ref 0 in
  let digits = Array.create ~len:10 0 in
  let is_used = Array.create ~len:10 false in
  let requirements =
    [| None; None; None; Some 2; Some 3; Some 5; Some 7; Some 11; Some 13; Some 17 |]
  in
  iter 0 digits ~is_used ~requirements ~f:(fun n ->
    if debug then Debug.eprintf "%d" n;
    sum := !sum + n);
  !sum
;;

let main () = sum_pandigital () |> printf "%d\n"

(* 2.789ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 16695334890 |}]
;;

include (val Solution.make ~problem:(Number 43) ~main)
