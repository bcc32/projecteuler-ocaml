open! Core
open! Import

module M = struct
  let problem = Number 3
  let main () = Number_theory.Int.factor 600851475143 |> List.last_exn |> printf "%d\n"

  let%expect_test "answer" =
    main ();
    [%expect {| 6857 |}]
  ;;
end

include Solution.Make (M)
