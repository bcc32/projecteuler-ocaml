open! Core
open! Import

let problem = Number 33

let main () =
  let fractions =
    let open Sequence.Let_syntax in
    let%bind n1 = Sequence.range 1 10 in
    let%bind n2 = Sequence.range 0 10 in
    let%bind d1 = Sequence.range 1 10 in
    let%bind d2 = Sequence.range 0 10 in
    let n = (10 * n1) + n2 in
    let d = (10 * d1) + d2 in
    if n >= d || (n2 = 0 && d2 = 0)
    then Sequence.empty
    else if n1 = d2 && Bignum.(n2 // d1 = n // d)
    then (
      (* this case isn't used *)
      if debug then Debug.eprintf "case 1: %d/%d = %d/%d" n d n2 d1;
      return Bignum.(n // d))
    else if n2 = d1 && Bignum.(n1 // d2 = n // d)
    then (
      if debug then Debug.eprintf "case 2: %d/%d = %d/%d" n d n1 d2;
      return Bignum.(n // d))
    else Sequence.empty
  in
  fractions
  |> Sequence.fold ~init:Bignum.one ~f:Bignum.( * )
  |> Bignum.den_as_bigint
  |> printf !"%{Bigint}\n"
;;

(* 0.926ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 100 |}]
;;

include (val Solution.make ~problem ~main)
