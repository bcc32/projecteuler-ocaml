open! Core
open! Import

let extend_right prime =
  [ 1; 3; 5; 7; 9 ]
  |> List.map ~f:(fun d -> (prime * 10) + d)
  |> List.filter ~f:Number_theory.Int.is_prime
;;

let search start ~step =
  let rec loop result queue ~step =
    match Fqueue.dequeue queue with
    | None -> result
    | Some (hd, tl) ->
      step hd |> List.fold ~init:tl ~f:Fqueue.enqueue |> loop (Set.add result hd) ~step
  in
  loop (Set.empty (module Int)) (Fqueue.of_list start) ~step
;;

let is_left_truncatable n =
  let pop_left_digit n =
    let rec loop power_of_ten =
      if power_of_ten * 10 > n then n mod power_of_ten else loop (power_of_ten * 10)
    in
    loop 1
  in
  let rec loop n =
    if n < 10 && (n = 2 || n = 3 || n = 5 || n = 7)
    then true
    else Number_theory.Int.is_prime n && loop (pop_left_digit n)
  in
  loop n
;;

let main () =
  search [ 2; 3; 5; 7 ] ~step:extend_right
  |> Fn.flip Set.diff (Set.of_list (module Int) [ 2; 3; 5; 7 ])
  |> Set.filter ~f:is_left_truncatable
  |> Set.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
;;

(* 9.74066ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 748317 |}]
;;

include (val Solution.make ~problem:(Number 37) ~main)
