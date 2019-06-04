open! Core
open! Import

let totient_chain_length limit =
  let table = Option_array.create ~len:(limit + 1) in
  Option_array.set_some table 1 1;
  let rec totient_chain_length n =
    if Option_array.is_some table n
    then Option_array.get_some_exn table n
    else (
      let result = totient_chain_length (Number_theory.Int.totient n) + 1 in
      Option_array.set_some table n result;
      result)
  in
  stage totient_chain_length
;;

let problem = Number 214
let limit = 40_000_000

let main () =
  let totient_chain_length = unstage (totient_chain_length limit) in
  debug_timing ~task:"sieving" Number_theory.prime_sieve limit
  |> Array.foldi ~init:0 ~f:(fun i acc is_prime ->
    if is_prime && totient_chain_length i = 25 then acc + i else acc)
  |> printf "%d\n"
;;

(* 1677366278943
   46.0785s *)

include (val Solution.make ~problem ~main)
