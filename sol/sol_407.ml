open! Core
open! Import

let problem = Number 407
let limit = 10_000_000

(* Modified solution inspired by umu:
   https://projecteuler.net/thread=407;post=92348. *)

(* a^2 = a (mod n) means a^2-a = a(a - 1) is divisible by n. Since a and (a -
   1) are coprime, and a < n, n = xy for 2 <= x, y and x|a, y|(a - 1). *)

let main () =
  let ms = Option_array.create ~len:(limit + 1) in
  let somes = ref 0 in
  let all = ref 0 in
  let rec loop m m_divisors m_pred_divisors =
    if m < 2
    then ()
    else (
      if debug && m mod 10_000 = 0
      then
        Debug.eprintf !"%d %{Percent}" m (Percent.of_mult (float !somes /. float !all));
      List.iter m_divisors ~f:(fun x ->
        List.iter m_pred_divisors ~f:(fun y ->
          let n = x * y in
          if m < n && n <= limit && not (Option_array.unsafe_is_some ms n)
          then (
            Option_array.unsafe_set_some ms n m;
            incr somes)));
      incr all;
      loop (m - 1) m_pred_divisors (Number_theory.Int.divisors (m - 2)))
  in
  loop limit (Number_theory.Int.divisors limit) (Number_theory.Int.divisors (limit - 1));
  let sum = ref 0 in
  (* M(1) = 0 *)
  for i = 2 to limit do
    if Option_array.unsafe_is_some ms i
    then sum := !sum + Option_array.unsafe_get_some_exn ms i
    else sum := !sum + 1 (* 1^2 = 1 (mod n) trivially for any n *)
  done;
  printf "%d\n" !sum
;;

(* 39782849136421
   42.3606s *)

include (val Solution.make ~problem ~main)
