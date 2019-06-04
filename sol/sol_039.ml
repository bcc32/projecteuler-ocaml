open! Core
open! Import

let problem = Number 39

let main () =
  Sequence.range ~stop:`inclusive 12 1000
  |> Sequence.map ~f:(fun p ->
    let solutions = ref 0 in
    for c = 5 to (p / 2) - 1 do
      for b = (p - c) / 2 to c - 1 do
        let a = p - c - b in
        if Geometry.is_pythagorean_triple a b c then incr solutions
      done
    done;
    p, !solutions)
  |> Sequence.max_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> uw
  |> fst
  |> printf "%d\n"
;;

(* 40.293ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 840 |}]
;;

include (val Solution.make ~problem ~main)
