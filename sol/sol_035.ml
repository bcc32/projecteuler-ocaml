open! Core
open! Import

module M = struct
  let problem = `Number 35
  let rotate ds = Doubly_linked.last_elt ds |> uw |> Doubly_linked.move_to_front ds

  let prime_circle n =
    let digits = Number_theory.Int.digits_of_int n |> Doubly_linked.of_list in
    let len = Doubly_linked.length digits in
    let results = Array.create n ~len in
    with_return (fun { return } ->
      for i = 0 to len - 1 do
        let n = Doubly_linked.to_sequence digits |> Number_theory.Int.int_of_digits in
        if Number_theory.Int.is_prime n
        then (
          results.(i) <- n;
          rotate digits)
        else return None
      done;
      Some results)
  ;;

  let main () =
    let circular_primes = Int.Hash_set.create () in
    for n = 2 to 1_000_000 do
      if not (Hash_set.mem circular_primes n)
      then Option.iter (prime_circle n) ~f:(Array.iter ~f:(Hash_set.add circular_primes))
    done;
    printf "%d\n" (Hash_set.length circular_primes)
  ;;

  (* 55
     2.8s *)
end

include Solution.Make (M)
