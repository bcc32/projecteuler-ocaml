open! Core
open! Import

let limit = 1_000_000
let collatz n = if n mod 2 = 0 then n / 2 else (3 * n) + 1

let rec collatz_length =
  let cache =
    lazy
      (let cache = Option_array.create ~len:(limit + 1) in
       Option_array.set_some cache 1 1;
       cache)
  in
  fun n ->
    let cache = force cache in
    if n <= limit && Option_array.is_some cache n
    then Option_array.unsafe_get_some_exn cache n
    else (
      let length = collatz_length (collatz n) + 1 in
      if n <= limit then Option_array.unsafe_set_some cache n length;
      length)
;;

let main () =
  Sequence.range ~stop:`inclusive 1 limit
  |> Sequence.max_elt ~compare:(fun a b ->
    Int.compare (collatz_length a) (collatz_length b))
  |> Option.value_exn
  |> printf "%d\n"
;;

(* 94ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 837799 |}]
;;

include (val Solution.make ~problem:(Number 14) ~main)
