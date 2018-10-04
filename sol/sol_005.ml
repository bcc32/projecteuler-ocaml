open! Core
open! Import

module M = struct
  let problem = Number 5

  let main () =
    List.range ~stop:`inclusive 1 20
    |> List.fold ~init:1 ~f:Number_theory.Int.lcm
    |> printf "%d\n"
  ;;

  let%expect_test "answer" =
    main ();
    [%expect {| 232792560 |}]
  ;;
end

include Solution.Make (M)
