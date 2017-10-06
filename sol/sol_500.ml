open Core

module M = struct
  let problem_number = 500

  let modulo = 500_500_507

  (* We know [PrimePi[7_400_000] >= 500_500] so this includes all first
     500,000 primes, which is the upper bound on the primes used. *)
  let upper_bound = 7_400_000

  let main () =
    let queue =
      Euler.prime_sieve upper_bound
      |> Array.filter_mapi ~f:(fun i p -> Option.some_if p i)
      |> Heap.of_array ~cmp:Int.compare
    in
    let number = ref 1 in
    for _ = 1 to 500_500 do
      let factor = Heap.pop_exn queue in
      number := !number * factor mod modulo;
      let factor = factor * factor in
      if factor < upper_bound
      then (Heap.add queue factor)
    done;
    !number
    |> printf "%d\n"
    (* 35407281, 500ms *)
end
include Euler.Solution.Make(M)
