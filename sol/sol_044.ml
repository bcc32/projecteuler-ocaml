open! Core
open! Import

(* https://en.wikipedia.org/wiki/Pentagonal_number#Tests_for_pentagonal_numbers *)
let is_pentagonal n =
  let s = Number_theory.Int.isqrt (1 + (24 * n)) in
  s * s = 1 + (24 * n) && s mod 6 = 5
;;

let main () =
  with_return (fun { return } ->
    for k = 1 to 1_000_000 do
      for j = k - 1 downto 1 do
        let pk = k * ((3 * k) - 1) / 2 in
        let pj = j * ((3 * j) - 1) / 2 in
        if is_pentagonal (pk + pj) && is_pentagonal (pk - pj) then return (pk - pj)
      done
    done;
    assert false)
  |> printf "%d\n"
;;

(* 25.063ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 5482660 |}]
;;

include (val Solution.make ~problem:(Number 44) ~main)
