open! Core
open! Import

let problem = Number 25
let threshold = Bigint.(pow (of_int 10) (of_int 999))

let main () =
  Sequence.findi Number_theory.Bigint.fibonacci ~f:(fun _ f -> Bigint.(f >= threshold))
  |> uw
  |> fst
  |> printf "%d\n"
;;

(* 1.692ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 4782 |}]
;;

include (val Solution.make ~problem ~main)
