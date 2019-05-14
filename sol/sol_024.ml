open! Core
open! Import

module M = struct
  let problem = Number 24
  let elements = 10
  let index = 999_999

  let main () =
    let rec loop i index candidates =
      if Set.is_empty candidates
      then []
      else (
        let elements_after_ith = elements - i - 1 in
        let permutations = Number_theory.Int.factorial elements_after_ith in
        let nth_at_i = index / permutations in
        let x = Set.nth candidates nth_at_i |> Option.value_exn ~here:[%here] in
        x :: loop (i + 1) (index % permutations) (Set.remove candidates x))
    in
    loop
      0
      index
      (Set.of_increasing_iterator_unchecked (module Int) ~len:elements ~f:Fn.id)
    |> List.map ~f:Int.to_string
    |> String.concat
    |> printf "%s\n"
  ;;

  (* 135ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 2783915460 |}]
  ;;
end

include Solution.Make (M)
