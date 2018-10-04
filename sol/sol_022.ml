open! Core
open! Import

module M = struct
  let problem = Number 22

  let names =
    lazy
      (Problem_022.data
       |> Parse.comma_separated_quoted_words
       |> List.sort ~compare:String.compare)
  ;;

  let letter_score ch = Char.to_int ch - Char.to_int 'A' + 1
  let name_score name = String.to_list name |> List.sum (module Int) ~f:letter_score

  let main () =
    let names = force names in
    List.mapi names ~f:(fun i name -> name_score name * (i + 1))
    |> List.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;

  (* 5.4ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 871198282 |}]
  ;;
end

include Solution.Make (M)
