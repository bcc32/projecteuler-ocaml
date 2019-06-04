open! Core
open! Import

let rec f =
  let cache = Hashtbl.create (module Bigint) in
  fun n ->
    Hashtbl.findi_or_add cache n ~default:(fun n ->
      let open Bigint.O in
      if n = zero
      then Bigint.of_int 1
      else if n % Bigint.of_int 2 = zero
      then f (n asr 1) + f (Bigint.pred (n asr 1))
      else f (n asr 1))
;;

let problem = Number 169
let main () = f Bigint.(pow (of_int 10) (of_int 25)) |> printf !"%{Bigint}\n"

(* 178653872807
   0.826ms *)

include (val Solution.make ~problem ~main)
