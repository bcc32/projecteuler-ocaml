open Core

module M = struct
  let problem_number = 500

  let modulo = 500_500_507

  let power_of_two_divisors k =
    (* We know [PrimePi[8_000_000] >= 500_500] so this includes all first
       500,000 primes, which is the upper bound on the primes used. *)
    let queue =
      Euler.prime_sieve 8_000_000
      |> Array.filter_mapi ~f:(fun i p -> Option.some_if p i)
      |> Heap.of_array ~cmp:Int.compare
    in
    let number = ref 1 in
    for _ = 1 to k do
      let factor = Heap.pop_exn queue in
      number := !number * factor mod modulo;
      Heap.add queue (factor * factor)
    done;
    !number

  let main () =
    power_of_two_divisors 500_500
    |> printf "%d\n"
    (* 35407281, 672ms *)
end
include Solution.Make(M)
