open! Core
open! Import

module M = struct
  let problem = Number 27

  type answer =
    { mutable a : int
    ; mutable b : int
    ; mutable number_of_primes : int
    }

  let count_primes a b =
    let rec loop n count =
      if Number_theory.Int.is_prime ((n * n) + (a * n) + b)
      then loop (n + 1) (count + 1)
      else count
    in
    loop 0 0
  ;;

  let main () =
    let answer = { a = 0; b = 0; number_of_primes = 0 } in
    for a = -999 to 999 do
      for b = -1000 to 1000 do
        let primes = count_primes a b in
        if primes > answer.number_of_primes
        then (
          answer.a <- a;
          answer.b <- b;
          answer.number_of_primes <- primes)
      done
    done;
    printf "%d\n" (answer.a * answer.b)
  ;;

  (* -59231
     625.911ms *)
end

include Solution.Make (M)
