open! Core
open! Import

let is_palindrome n =
  let s = Bigint.to_string n in
  String.equal s (String.rev s)
;;

let is_lychrel n =
  Sequence.unfold_step ~init:n ~f:(fun n ->
    let next = Bigint.(n + (n |> to_string |> String.rev |> of_string)) in
    Yield { value = next; state = next })
  |> Fn.flip Sequence.take 50
  |> Sequence.exists ~f:is_palindrome
  |> not
;;

let main () =
  Sequence.range 1 10_000
  |> Sequence.map ~f:Bigint.of_int
  |> Sequence.count ~f:is_lychrel
  |> printf "%d\n"
;;

(* 38.775ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 249 |}]
;;

include (val Solution.make ~problem:(Number 55) ~main)
