open! Core
open! Import

module M = struct
  let problem = `Number 407
  let limit = 10_000_000
  let _primes = lazy (Number_theory.prime_sieve limit)

  let largest_idempotent modulo =
    Debug.eprintf "%d" modulo;
    with_return (fun { return } ->
      (* if (force primes).(modulo)
       * then (return 1); *)
      for a = modulo - 1 downto 0 do
        if a * a mod modulo = a then return a
      done;
      assert false)
  ;;

  let main () =
    Sequence.range 1 limit ~stop:`inclusive
    |> Sequence.sum (module Int) ~f:largest_idempotent
    |> printf "%d\n"
  ;;

  (* let max_idempotent = Array.create 0 ~len:(limit + 1)
   *
   * let compute () =
   *   for a = limit - 1 downto 1 do
   *     let x = a * a - a in
   *     for j = a + 1 to min x limit do
   *       if max_idempotent.(j) = 0 && x mod j = 0
   *       then (max_idempotent.(j) <- a)
   *     done
   *   done
   *
   * let main () =
   *   compute ();
   *   max_idempotent
   *   |> Array.sum (module Int) ~f:Fn.id
   *   |> printf "%d\n" *)
end

include Solution.Make (M)
