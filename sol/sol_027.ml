open! Core
open! Import

module M = struct
  let problem = `Number 27

  let count_primes a b =
    let rec loop n count =
      if Number_theory.Int.is_prime ((n * n) + (a * n) + b)
      then loop (n + 1) (count + 1)
      else count
    in
    loop 0 0
  ;;

  let main () =
    let max = ref None in
    for a = -999 to 999 do
      for b = -999 to 999 do
        match !max with
        | None -> max := Some (a, b, count_primes a b)
        | Some (_, _, primes') ->
          let primes = count_primes a b in
          if primes > primes' then max := Some (a, b, primes)
      done
    done;
    let a, b, _ = uw !max in
    printf "%d\n" (a * b)
  ;;

  (* -59231
     926ms *)
end

include Solution.Make (M)
