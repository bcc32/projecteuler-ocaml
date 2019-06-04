open! Core
open! Import

let problem = Number 9

let main () =
  with_return_option (fun r ->
    for c = 5 to 500 do
      for b = 4 to c - 1 do
        let a = 1000 - b - c in
        if Geometry.is_pythagorean_triple a b c then r.return (a * b * c)
      done
    done)
  |> Option.value_exn
  |> printf "%d\n"
;;

let%expect_test "answer" =
  main ();
  [%expect {| 31875000 |}]
;;

include (val Solution.make ~problem ~main)
