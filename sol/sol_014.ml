open Core

module M = struct
  let problem_number = 14

  let collatz n =
    if n mod 2 = 0
    then n / 2
    else 3 * n + 1

  let collatz_length n =
    let rec iter n acc =
      match n with
      | 1 -> acc
      | n -> iter (collatz n) (acc + 1)
    in
    iter n 1

  let main () =
    let argmax = ref 0 in
    let max    = ref 0 in
    for i = 1 to 1000000 do
      let len = collatz_length i in
      if len > !max
      then (
        argmax := i;
        max    := len
      )
    done;
    printf "%d\n" !argmax
end

include Euler.Solution.Make(M)
