open! Core
open! Import

let collatz n = if n mod 2 = 0 then n / 2 else (3 * n) + 1

let rec collatz_length =
  let cache = Hashtbl.create (module Int) in
  fun n ->
    Hashtbl.findi_or_add cache n ~default:(fun n ->
      match n with
      | 1 -> 1
      | n -> 1 + collatz_length (collatz n))
;;

let main () =
  Sequence.range ~stop:`inclusive 1 1000000
  |> Sequence.max_elt ~compare:(fun a b ->
    Int.compare (collatz_length a) (collatz_length b))
  |> Option.value_exn
  |> printf "%d\n"
;;

(* 837799
   3.37354s *)
include (val Solution.make
               ~problem:
                 (Tagged
                    { number = 14
                    ; tag = "memo"
                    ; description = "slower method using hashtbl-memoized collatz_length"
                    })
               ~main)
