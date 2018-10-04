open! Core
open! Import

module M = struct
  let problem = Number 6
  let hund = List.range ~stop:`inclusive 1 100
  let sum = List.sum (module Int) ~f:Fn.id
  let sqr x = x * x
  let main () = sqr (sum hund) - sum (List.map ~f:sqr hund) |> printf "%d\n"

  let%expect_test "answer" =
    main ();
    [%expect {| 25164150 |}]
  ;;
end

include Solution.Make (M)
