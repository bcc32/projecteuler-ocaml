open Core

module M = struct
  let problem_number = 500

  let modulo = 500_500_507

  (* We know [PrimePi[7_400_000] >= 500_500] so this includes all first
     500,000 primes, which is the upper bound on the primes used. *)
  let upper_bound = 7_400_000

  let main () =
    let sieve = Array.create false ~len:upper_bound in
    let rec loop i ac count =
      if count >= 500_500
      then ac
      else (
        if not sieve.(i)
        then (
          let count = count + 1 in
          let ac = ac * i mod modulo in
          let rec mark j =
            if j < upper_bound
            then (
              sieve.(j) <- true;
              mark (j + i))
          in
          mark (2 * i);
          if i * i < upper_bound
          then (sieve.(i * i) <- false);
          loop (i + 1) ac count)
        else (
          loop (i + 1) ac count))
    in
    loop 2 1 0
    |> printf "%d\n"
    (* 35407281, 325ms *)
end
include Solution.Make(M)
