open! Core
open! Import

let of_digits = Array.to_sequence_mutable >> Number_theory.Int.As_base10.of_sequence

let connected_primes limit =
  let is_prime = Number_theory.prime_sieve limit in
  stage
    ( is_prime
    , fun ~from:base_prime ->
      let digits = Number_theory.Int.As_base10.to_array base_prime in
      let open Sequence.Let_syntax in
      let digit_changed =
        let%bind i = Sequence.range 0 (Array.length digits) in
        let base_prime_digit = digits.(i) in
        let%map other_digit =
          Sequence.range 0 10 |> Sequence.filter ~f:(fun d -> d <> base_prime_digit)
        in
        Array.mapi digits ~f:(fun j x -> if i = j then other_digit else x) |> of_digits
      in
      let digit_added =
        let%map new_digit = Sequence.range 0 10 in
        Array.init
          (Array.length digits + 1)
          ~f:(fun i -> if i = 0 then new_digit else digits.(i - 1))
        |> of_digits
      in
      let digit_removed =
        if Array.length digits > 1
        then return (Array.subo digits ~pos:1 |> of_digits)
        else Sequence.empty
      in
      Sequence.append (Sequence.append digit_changed digit_added) digit_removed
      |> Sequence.filter ~f:(fun p -> p < Array.length is_prime && is_prime.(p))
      |> Sequence.fold ~init:(Set.empty (module Int)) ~f:Set.add )
;;

let%expect_test _ =
  let _, connected_primes = unstage (connected_primes 10_000) in
  connected_primes ~from:123 |> [%sexp_of: Set.M(Int).t] |> print_s;
  [%expect {| (23 103 113 127 163 173 193 223 523 823 1123 8123) |}]
;;

module Priority_queue = Hash_heap.Make (Int)

exception All_2's_relatives_found

let sum_primes_not_2's_relatives limit =
  let is_prime, connected_primes = unstage (connected_primes limit) in
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
       connected_primes ~from:n
       |> Set.iter ~f:(fun p ->
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

module M = struct
  let problem = Number 425
  let limit = 10_000_000
  let main () = sum_primes_not_2's_relatives limit |> printf "%d\n"

  (* 46479497324
     15.692287046s *)
end

include Solution.Make (M)
