open! Core
open! Import

module M = struct
  let problem = Number 50
  let limit = 1_000_000

  let main () =
    let is_prime = Number_theory.prime_sieve limit in
    let primes =
      Array.filter_mapi is_prime ~f:(fun i is_prime -> Option.some_if is_prime i)
    in
    let max = ref 0 in
    let arg_max = ref 0 in
    for pos = 0 to Array.length primes do
      if is_prime.(pos)
      then (
        let sum = ref 0 in
        try
          for len = 1 to Array.length primes - pos do
            sum := !sum + primes.(pos + len - 1);
            if !sum > limit then raise Exit;
            if len > !max && is_prime.(!sum)
            then (
              max := len;
              arg_max := !sum)
          done
        with
        | Exit -> ())
    done;
    printf "%d\n" !arg_max
  ;;

  (* 997651
     79.818ms *)
end

include Solution.Make (M)
