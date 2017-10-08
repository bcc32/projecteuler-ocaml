open! Core

module M = struct
  let problem_number = 12

  let triangle n = n * (n + 1) / 2

  let main () =
    let i = ref 1 in
    while (Euler.Int.num_divisors (triangle !i)) <= 500 do
      Int.incr i
    done;
    printf "%d\n" (triangle !i)
  ;;
end

include Euler.Solution.Make(M)
