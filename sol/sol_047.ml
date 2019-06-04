open! Core
open! Import

let problem = Number 47

let main () =
  let distinct_prime_factors =
    Number_theory.Int.natural_numbers ~init:2 ()
    |> Sequence.map ~f:(fun n ->
      if debug then Debug.eprintf "factoring %d" n;
      n, Number_theory.Int.prime_factor n |> List.length)
  in
  let each_consecutive_4 =
    let dpf = distinct_prime_factors in
    let a, dpf = uw (Sequence.next dpf) in
    let b, dpf = uw (Sequence.next dpf) in
    let c, dpf = uw (Sequence.next dpf) in
    Sequence.folding_map dpf ~init:(a, b, c) ~f:(fun (a, b, c) d ->
      (b, c, d), (a, b, c, d))
  in
  let rec loop seq =
    let ((a, apf), (_, bpf), (_, cpf), (_, dpf)), seq = uw (Sequence.next seq) in
    (* Check [dpf] first à la Boyer-Moore:
       https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm.

       This doesn't actually buy us much performance-wise, but it's cool, so
       whatever. *)
    let step n = loop (Sequence.drop_eagerly seq n) in
    if dpf <> 4
    then step 3
    else if cpf <> 4
    then step 2
    else if bpf <> 4
    then step 1
    else if apf <> 4
    then step 0
    else a
  in
  loop each_consecutive_4 |> printf "%d\n"
;;

(* 98.823ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 134043 |}]
;;

include (val Solution.make ~problem ~main)
