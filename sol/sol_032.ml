open! Core
open! Import

(* WLOG a < b, so a has at most 2 digits *)
let pandigital () =
  let expected_digits = List.range 1 10 in
  let merge n ~into:sorted_digits =
    List.merge
      ~compare:Int.compare
      (List.sort ~compare:Int.compare (Number_theory.Int.to_digits n))
      sorted_digits
  in
  let has_consecutive_duplicate =
    Option.is_some << List.find_consecutive_duplicate ~equal:Int.equal
  in
  let open Sequence.Let_syntax in
  let%bind a = Sequence.range 1 100 in
  let digits = merge a ~into:[] in
  if has_consecutive_duplicate digits
  then Sequence.empty
  else (
    let%bind b = Sequence.range (a + 1) (Number_theory.Int.isqrt (1_000_000_000 / a)) in
    let digits = merge b ~into:digits in
    if has_consecutive_duplicate digits
    then Sequence.empty
    else (
      let c = a * b in
      let digits = merge c ~into:digits in
      if [%equal: int list] expected_digits digits then return c else Sequence.empty))
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

  (* 200ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 45228 |}]
  ;;
end

include Solution.Make (M)
