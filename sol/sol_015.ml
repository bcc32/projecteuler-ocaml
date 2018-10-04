open! Core
open! Import

module M = struct
  let problem = Number 15

  let main () =
    let n = Bigint.of_int 40 in
    let r = Bigint.of_int 20 in
    Number_theory.Bigint.binomial n r |> printf !"%{Bigint}\n"
  ;;

  let%expect_test "answer" =
    main ();
    [%expect {| 137846528820 |}]
  ;;
end

include Solution.Make (M)
