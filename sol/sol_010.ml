open! Core
open! Import

let problem = Number 10

let main () =
  Number_theory.prime_sieve 2_000_000
  |> Array.foldi ~init:0 ~f:(fun n acc is_prime -> if is_prime then acc + n else acc)
  |> printf "%d\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 142913828922 |}]
;;

include (val Solution.make ~problem ~main)
