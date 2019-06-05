open! Core
open! Import

let main () =
  let limit = Bigint.of_int 1_000_000 in
  Sequence.(
    range 1 100 ~stop:`inclusive
    |> concat_map ~f:(fun n -> range 0 n ~stop:`inclusive |> map ~f:(fun r -> n, r))
    |> map ~f:(fun (n, r) ->
      let n = Bigint.of_int n in
      let r = Bigint.of_int r in
      Number_theory.Bigint.binomial n r)
    |> count ~f:(Bigint.( < ) limit))
  |> printf "%d\n"
;;

(* 15.168ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 4075 |}]
;;

include (val Solution.make ~problem:(Number 53) ~main)
