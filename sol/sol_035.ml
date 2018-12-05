open! Core
open! Import

module M = struct
  let problem = Number 35
  let limit = 999_999
  let is_prime = lazy (Number_theory.prime_sieve limit)

  let prime_circle n =
    let digits = Number_theory.Int.digits_of_int n |> Array.of_list in
    let len = Array.length digits in
    let double_digits = Array.append digits digits in
    let results = ref [] in
    let rec loop i =
      if i < len
      then (
        let n =
          let rec loop pos end_pos acc =
            if pos < end_pos
            then loop (pos + 1) end_pos ((10 * acc) + double_digits.(pos))
            else acc
          in
          loop i (i + len) 0
        in
        if (force is_prime).(n)
        then (
          results := n :: !results;
          loop (i + 1))
        else None)
      else Some !results
    in
    loop 0
  ;;

  let main () =
    let circular_primes = Int.Hash_set.create () in
    for n = 2 to limit do
      if not (Hash_set.mem circular_primes n)
      then Option.iter (prime_circle n) ~f:(List.iter ~f:(Hash_set.add circular_primes))
    done;
    printf "%d\n" (Hash_set.length circular_primes)
  ;;

  (* TODO update timing *)
  (* 55
     835.332ms *)
end

include Solution.Make (M)
