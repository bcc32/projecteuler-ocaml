open! Core
open! Import

let threshold = Bigint.(pow (of_int 10) (of_int 999))

let main () =
  Sequence.findi Number_theory.Bigint.fibonacci ~f:(fun _ f -> Bigint.(f >= threshold))
  |> Option.value_exn
  |> fst
  |> printf "%d\n"
;;

(* 1.692ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 4782 |}]
;;

include (val Solution.make ~problem:(Number 25) ~main)
