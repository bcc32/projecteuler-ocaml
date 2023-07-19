open! Core
open! Import

let cannot_be_written n =
  let upper_bound = Number_theory.Int.isqrt (n / 2) in
  not
    (Sequence.range ~stop:`inclusive 1 upper_bound
     |> Sequence.exists ~f:(fun s -> n - (2 * s * s) |> Number_theory.Int.is_prime))
;;

let main () =
  Sequence.unfold_step ~init:3 ~f:(fun s -> Yield { value = s; state = s + 2 })
  |> Sequence.filter ~f:(Fn.non Number_theory.Int.is_prime)
  |> Sequence.find_exn ~f:cannot_be_written
  |> printf "%d\n"
;;

(* 5.236ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 5777 |}]
;;

include (val Solution.make ~problem:(Number 46) ~main)
