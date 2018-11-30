open! Core
open! Import

module M = struct
  let problem = Number 35
  let limit = 1_000_000
  let rotate ds = Doubly_linked.last_elt ds |> uw |> Doubly_linked.move_to_front ds
  let is_prime = lazy (Number_theory.prime_sieve limit)

  let prime_circle n =
    let digits = Number_theory.Int.digits_of_int n |> Doubly_linked.of_list in
    let len = Doubly_linked.length digits in
    let results = ref [] in
    let rec loop rotations =
      let n = Doubly_linked.fold digits ~init:0 ~f:(fun acc d -> (10 * acc) + d) in
      if (force is_prime).(n)
      then (
        results := n :: !results;
        if rotations > 0
        then (
          rotate digits;
          loop (rotations - 1))
        else Some !results)
      else None
    in
    loop (len - 1)
  ;;

  let main () =
    let circular_primes = Int.Hash_set.create () in
    for n = 2 to limit do
      if not (Hash_set.mem circular_primes n)
      then Option.iter (prime_circle n) ~f:(List.iter ~f:(Hash_set.add circular_primes))
    done;
    printf "%d\n" (Hash_set.length circular_primes)
  ;;

  (* 55
     835.332ms *)
end

include Solution.Make (M)
