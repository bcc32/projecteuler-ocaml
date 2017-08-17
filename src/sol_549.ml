open Core

module M = struct
  let problem_number = 549

  let limit =
    match Sys.getenv "LIMIT" with
    | Some x -> Int.of_string x
    | None -> 100_000_000

  let percent = limit / 100

  let start = lazy (Time.now ())

  let primes =
    lazy (
      Euler.prime_sieve limit
      |> Array.filter_mapi ~f:(fun i p -> Option.some_if p i))

  let divide n p =
    let rec loop n ac =
      if n mod p = 0
      then loop (n / p) (ac + 1)
      else (n, ac)
    in
    loop n 0

  let s_prime_factor p k =
    let rec loop p k n ac =
      if ac >= k
      then n
      else (
        let (_, a) = divide (n + p) p in
        loop p k (n + p) (a + ac))
    in
    loop p k 0 0

  let s n =
    if n mod percent = 0
    then (
      let elapsed = Time.diff (Time.now ()) (force start) in
      Debug.eprintf "%d%% %{sexp: Time.Span.t}" (n / percent) elapsed);
    let max = ref 0 in
    let n = ref n in
    with_return (fun { return } ->
      Array.iter (force primes) ~f:(fun p ->
        if p * p > !n
        then (return (Int.max !max !n))
        else if !n mod p = 0
        then (
          let (n', k) = divide !n p in
          max := Int.max !max (s_prime_factor p k);
          n := n'));
      !max)

  let total_s n =
    Sequence.range 2 n ~stop:`inclusive
    |> Sequence.sum (module Int) ~f:s

  let main () =
    Debug.eprint "precomputing primes...";
    ignore (force primes : int array);
    Debug.eprint "done";
    ignore (force start : Time.t);
    total_s limit
    |> printf "%d\n"
    (* 476001479068717, 7.6m *)
end

include Solution.Make(M)
