open! Core
open! Import

module M = struct
  let problem = Number 19

  let main () =
    let start = Date.of_string "1901-01-01" in
    let stop = Date.of_string "2000-12-31" in
    Sequence.unfold ~init:start ~f:(fun d -> Some (d, Date.add_months d 1))
    |> Sequence.take_while ~f:(fun x -> Date.O.(x <= stop))
    |> Sequence.count ~f:(fun d -> [%equal: Day_of_week.t] Sun (Date.day_of_week d))
    |> printf "%d\n"
  ;;

  (* 167.972us *)
  let%expect_test "answer" =
    main ();
    [%expect {| 171 |}]
  ;;
end

include Solution.Make (M)
