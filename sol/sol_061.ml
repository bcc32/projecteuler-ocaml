open! Core
open! Import

let limit = 10_000

let create_mask f =
  let mask = Array.create ~len:limit false in
  let rec loop x =
    let y = f x in
    if y >= limit
    then ()
    else (
      mask.(y) <- true;
      loop (x + 1))
  in
  loop 1;
  stage (fun n -> mask.(n))
;;

let find () =
  let is_p3 = unstage (create_mask (fun n -> n * (n + 1) / 2)) in
  let is_p4 = unstage (create_mask (fun n -> n * n)) in
  let is_p5 = unstage (create_mask (fun n -> n * ((3 * n) - 1) / 2)) in
  let is_p6 = unstage (create_mask (fun n -> n * ((2 * n) - 1))) in
  let is_p7 = unstage (create_mask (fun n -> n * ((5 * n) - 3) / 2)) in
  let is_p8 = unstage (create_mask (fun n -> n * ((3 * n) - 2))) in
  let ( ^^ ) d1 d2 = (d1 * 100) + d2 in
  with_return (fun { return } ->
    (* fix the first number to be triangular, iterate over permutations of other
       polygons *)
    Sequences.permutations
      ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      [ 4, is_p4; 5, is_p5; 6, is_p6; 7, is_p7; 8, is_p8 ]
    |> Sequence.iter ~f:(function
      | [ (_, pred1); (_, pred2); (_, pred3); (_, pred4); (_, pred5) ] ->
        (* iterate over digit pair combinations *)
        for d0 = 10 to 99 do
          for d1 = 10 to 99 do
            if is_p3 (d0 ^^ d1)
            then
              for d2 = 10 to 99 do
                if pred1 (d1 ^^ d2)
                then
                  for d3 = 10 to 99 do
                    if pred2 (d2 ^^ d3)
                    then
                      for d4 = 10 to 99 do
                        if pred3 (d3 ^^ d4)
                        then
                          for d5 = 10 to 99 do
                            if pred4 (d4 ^^ d5) && pred5 (d5 ^^ d0)
                            then
                              return
                                [ d0 ^^ d1
                                ; d1 ^^ d2
                                ; d2 ^^ d3
                                ; d3 ^^ d4
                                ; d4 ^^ d5
                                ; d5 ^^ d0
                                ]
                          done
                      done
                  done
              done
          done
        done
      | _ -> assert false);
    assert false)
;;

let main () =
  let numbers = find () in
  if debug then Debug.eprint_s [%sexp (numbers : int list)];
  numbers |> List.sum (module Int) ~f:Fn.id |> printf "%d\n"
;;

(* 2.025ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 28684 |}]
;;

include (val Solution.make ~problem:(Number 61) ~main)
