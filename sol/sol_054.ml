open! Core
open! Import

module M = struct
  let problem = Number 54

  let main () =
    Problem_054.data
    |> Parse.space_separated_grid ~conv:Poker.Card.of_string
    |> Array.map ~f:(fun line ->
      let make_hand cards = cards |> Array.to_list |> Poker.Hand.of_card_list_exn in
      let p1 = Array.subo line ~len:5 |> make_hand in
      let p2 = Array.subo line ~pos:5 |> make_hand in
      p1, p2)
    |> Array.count ~f:(fun (p1, p2) ->
      Poker.Hand_classification.( > )
        (Poker.Hand.classify p1)
        (Poker.Hand.classify p2))
    |> printf "%d\n"
  ;;

  (* 6.821ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 376 |}]
  ;;
end

include Solution.Make (M)
