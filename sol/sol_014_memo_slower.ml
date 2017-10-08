open! Core

let collatz n =
  if n mod 2 = 0
  then n / 2
  else 3 * n + 1
;;

let rec collatz_length =
  let cache = Int.Table.create () in
  fun n ->
    Int.Table.find_or_add cache n
      ~default:(fun () ->
        match n with
        | 1 -> 1
        | n -> 1 + (collatz_length (collatz n)))
;;

let () =
  Sequence.range ~stop:`inclusive 1 1000000
  |> Sequence.max_elt ~cmp:(fun a b ->
    Int.compare
      (collatz_length a)
      (collatz_length b))
  |> Option.value_exn
  |> printf "%d\n"
;;
