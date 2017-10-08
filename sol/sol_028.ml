open! Core

module M = struct
  let problem_number = 28

  let main () =
    let i   = ref 1 in
    let di  = ref 2 in
    let sum = ref 1 in
    for _ = 1 to 500 do
      for _ = 1 to 4 do
        i   := !i   + !di;
        sum := !sum + !i;
      done;
      di := !di + 2;
    done;
    printf "%d\n" !sum
  ;;
end

include Euler.Solution.Make(M)
