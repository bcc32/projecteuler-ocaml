open! Core
open! Import

module Digit_set : sig
  type t [@@deriving compare, hash, sexp_of]

  val of_int : int -> t
end = struct
  type t = int list [@@deriving compare, hash, sexp_of]

  let of_int int =
    Number_theory.Int.As_base10.to_list int |> List.sort ~compare:Int.compare
  ;;
end

let find min_permutations =
  let cubes_by_digit_set = Hashtbl.create (module Digit_set) in
  let[@inline] cube n = Number_theory.Int.addition_chain_pow n 3 in
  let rec loop n =
    let c = cube n in
    let ds = Digit_set.of_int c in
    Hashtbl.add_multi cubes_by_digit_set ~key:ds ~data:c;
    let cubes = Hashtbl.find_multi cubes_by_digit_set ds in
    if List.length cubes >= min_permutations
    then uw (List.min_elt cubes ~compare:Int.compare)
    else loop (n + 1)
  in
  loop 1
;;

let min_permutations = 5
let main () = find min_permutations |> printf "%d\n"

(* 29.618ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 127035954683 |}]
;;

include (val Solution.make ~problem:(Number 62) ~main)
