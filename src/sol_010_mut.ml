open Core.Std

module M = struct
  let problem_number = 10

  let max_prime = 2_000_000

  let primes =
    Array.create ~len:max_prime true

  let is_prime n =
    primes.(n)

  let rec mark p n =
    if n < max_prime
    then (
      primes.(n) <- false;
      mark p (n + p)
    )

  let rec sieve p =
    if p * p < max_prime
    then (
      if is_prime p
      then mark p (p * p);
      sieve (Euler.next_probable_prime p)
    )

  let main () =
    primes.(0) <- false;
    primes.(1) <- false;
    sieve 2;
    Array.foldi primes ~init:0 ~f:(fun n acc is_prime ->
      if is_prime
      then acc + n
      else acc
    )
    |> printf "%d\n"
end

include Solution.Make(M)
