open! Core

module M = struct
  let problem_number = 407

  let limit = 10_000_000

  let primes = lazy (Euler.prime_sieve limit)

  let largest_idempotent modulo =
    with_return (fun { return } ->
      if (force primes).(modulo)
      then (return 1);
      for a = modulo - 1 downto 0 do
        if a * a mod modulo = a
        then (return a)
      done;
      assert false)

  let main () =
    let sum = ref 0 in
    for n = 1 to limit do
      sum := !sum + largest_idempotent n
    done;
    printf "%d\n" !sum
end
include Solution.Make(M)
