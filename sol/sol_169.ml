open! Core
open! Import

(* Recurrence described by ed_r at https://projecteuler.net/thread=169#13152. *)
let f =
  Memo.recursive
    (module Bigint)
    (fun f n ->
      let open Bigint.O in
      if n = zero
      then Bigint.of_int 1
      else if n % Bigint.of_int 2 = zero
      then f (n asr 1) + f (Bigint.pred (n asr 1))
      else f (n asr 1))
;;

let main () = f Bigint.(pow (of_int 10) (of_int 25)) |> printf !"%{Bigint}\n"

(* TODO Check timing. *)
(* 0.826ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 178653872807 |}]
;;

include (val Solution.make ~problem:(Number 169) ~main)
