open! Core
open! Import

let of_digits = Number_theory.Int.As_base10.of_array

let iter_connected_primes limit =
  let is_prime = Number_theory.prime_sieve limit in
  stage
    ( is_prime
    , fun ~from:base_prime ~f ->
      let digits = Number_theory.Int.As_base10.to_array base_prime in
      let call_f_if_prime p = if p < Array.length is_prime && is_prime.(p) then f p in
      (* One digit changed *)
      for i = 0 to Array.length digits - 1 do
        let base_prime_digit = digits.(i) in
        for d = 0 to 9 do
          if d <> base_prime_digit
          then (
            digits.(i) <- d;
            call_f_if_prime (of_digits digits))
        done;
        digits.(i) <- base_prime_digit
      done;
      (* Digit added *)
      let digits_with_one_added =
        Array.init
          (Array.length digits + 1)
          ~f:(fun i -> if i = 0 then 0 else digits.(i - 1))
      in
      for new_digit = 0 to 9 do
        digits_with_one_added.(0) <- new_digit;
        call_f_if_prime (of_digits digits_with_one_added)
      done;
      (* Digit removed *)
      if Array.length digits > 1
      then call_f_if_prime (Array.subo digits ~pos:1 |> of_digits) )
;;

let%expect_test _ =
  let _, iter_connected_primes = unstage (iter_connected_primes 10_000) in
  let result = ref [] in
  iter_connected_primes ~from:123 ~f:(fun x -> result := x :: !result);
  List.dedup_and_sort !result ~compare:Int.compare |> [%sexp_of: int list] |> print_s;
  [%expect {| (23 103 113 127 163 173 193 223 523 823 1123 8123) |}]
;;

module Priority_queue = Hash_heap.Make (Int)

exception All_2's_relatives_found

let sum_primes_not_2's_relatives limit =
  let is_prime, iter_connected_primes = unstage (iter_connected_primes limit) in
  (* We define the distance between [2] and another prime [p] as the smallest
     possible value of [max(c) for c in chain(2, p)], including [p] itself.

     Happily, this distance metric weakly satisfies the triangle inequality
     (i.e., extending a path never decreases the distance), so it should be OK
     to use Dijkstra's algorithm here. *)
  let distance = Priority_queue.create Int.compare in
  let distance_known = Hashtbl.create (module Int) in
  Priority_queue.push_exn distance ~key:2 ~data:2;
  (try
     while Priority_queue.length distance > 0 do
       let n, d = Priority_queue.pop_with_key_exn distance in
       Hashtbl.add_exn distance_known ~key:n ~data:d;
       if d > limit
       then
         (* We know that all of 2's relatives under [limit] have been found now,
            since the chains of any following primes must contain something >
            limit. *)
         raise All_2's_relatives_found;
       iter_connected_primes ~from:n ~f:(fun p ->
         let new_distance_for_p = Int.max d p in
         if not (Hashtbl.mem distance_known p)
         && (not (Priority_queue.mem distance p)
             || Priority_queue.find_exn distance p > new_distance_for_p)
         then Priority_queue.replace distance ~key:p ~data:new_distance_for_p)
     done
   with
   | All_2's_relatives_found -> ());
  (* We don't eagerly sum these because we need to make sure to count primes
     that aren't even connected to 2 through any chain. *)
  let sum_not_related = ref 0 in
  for p = 2 to limit do
    if is_prime.(p)
    then (
      let related =
        Hashtbl.findi_and_call
          distance_known
          p
          ~if_found:(fun ~key:p ~data:d -> d <= p)
          ~if_not_found:(fun _ -> false)
      in
      if not related then sum_not_related := !sum_not_related + p)
  done;
  !sum_not_related
;;

let%expect_test "examples" =
  printf "%d\n" (sum_primes_not_2's_relatives 1_000);
  [%expect {| 431 |}];
  printf "%d\n" (sum_primes_not_2's_relatives 10_000);
  [%expect {| 78728 |}]
;;

let problem = Number 425
let limit = 10_000_000
let main () = sum_primes_not_2's_relatives limit |> printf "%d\n"

(* 46479497324
   8.729529377s *)

include (val Solution.make ~problem ~main)
