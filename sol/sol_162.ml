open! Core
open! Import

(* iterate over the possible positions of the first occurrences of each
   necessary digit. *)

module Necessary_digit = struct
  type t =
    | Zero
    | One
    | Ten
  [@@deriving compare, equal]
end

open Necessary_digit

module Element = struct
  type t =
    | Other
    | First_occurrence_of of Necessary_digit.t
  [@@deriving compare, equal]
end

open Element

let patterns digits =
  let pattern =
    List.init (digits - 3) ~f:(fun _ -> Other)
    @ [ First_occurrence_of Zero; First_occurrence_of One; First_occurrence_of Ten ]
  in
  Sequences.permutations pattern ~compare:Element.compare
  |> Sequence.filter ~f:(fun perm ->
    not ([%equal: Element.t] (List.hd_exn perm) (First_occurrence_of Zero)))
;;

let allowed_numbers digits =
  patterns digits
  |> Sequence.sum
       (module Int)
       ~f:(fun pattern ->
         List.fold pattern ~init:(1, 13) ~f:(fun (ac, allowed_digits) elt ->
           match elt with
           | Other -> ac * allowed_digits, allowed_digits
           | First_occurrence_of _ -> ac, allowed_digits + 1)
         |> fst)
;;

let main () =
  let limit = 16 in
  Sequence.range 3 limit ~stop:`inclusive
  |> Sequence.sum (module Int) ~f:allowed_numbers
  |> printf "%X\n"
;;

(* 6.268ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 3D58725572C62302 |}]
;;

include (val Solution.make ~problem:(Number 162) ~main)
