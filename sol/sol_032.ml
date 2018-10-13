open! Core
open! Import

(* WLOG a < b, so a has at most 3 digits (actually almost certainly fewer) *)
let pandigital () =
  let expected_digits = List.range 1 10 in
  let open Sequence.Let_syntax in
  let%bind a = Sequence.range 1 1000 in
  let digits = Number_theory.Int.digits_of_int a in
  let%bind b = Sequence.range (a + 1) 10_000 in
  let digits = digits @ Number_theory.Int.digits_of_int b in
  let c = a * b in
  let digits = digits @ Number_theory.Int.digits_of_int c in
  if [%compare.equal: int list] expected_digits (List.sort digits ~compare:Int.compare)
  then return c
  else Sequence.empty
;;

module M = struct
  let problem = Number 32

  let main () =
    pandigital ()
    |> Sequence.to_list
    |> Int.Set.of_list
    |> Set.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;

  (* 45228
     8.76643s *)
end

include Solution.Make (M)
