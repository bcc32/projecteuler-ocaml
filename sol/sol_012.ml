open! Core
open! Import

module M = struct
  let problem = `Number 12
  let triangle n = n * (n + 1) / 2

  let main () =
    let i = ref 1 in
    while Number_theory.Int.num_divisors (triangle !i) <= 500 do
      Int.incr i
    done;
    printf "%d\n" (triangle !i)
  ;;
end

include Solution.Make (M)
