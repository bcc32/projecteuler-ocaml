open! Core
open! Import

module M = struct
  let problem = `Number 10

  let main () =
    Number_theory.prime_sieve 2_000_000
    |> Array.foldi ~init:0 ~f:(fun n acc is_prime -> if is_prime then acc + n else acc)
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
