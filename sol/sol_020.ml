open! Core
open! Import

module M = struct
  let problem = Number 20

  let main () =
    Bigint.of_int 100
    |> Number_theory.Bigint.factorial
    |> Number_theory.Bigint.sum_digits
    |> printf !"%{Bigint}\n"
  ;;

  let%expect_test "answer" =
    main ();
    [%expect {| 648 |}]
  ;;
end

include Solution.Make (M)
