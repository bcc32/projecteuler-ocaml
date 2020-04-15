open! Core
open! Import

module State = struct
  type t =
    { heap_size : int
    ; last_taken : int
    }
  [@@deriving compare, hash, sexp_of]
end

let can_win =
  Memo.recursive
    (module State)
    (fun can_win ({ heap_size; last_taken } : State.t) ->
       if heap_size = 0
       then false
       else if heap_size <= last_taken * 2
       then true
       else
         Sequence.range 1 (last_taken * 2) ~stop:`inclusive
         |> Sequence.exists ~f:(fun move ->
           not (can_win { heap_size = heap_size - move; last_taken = move })))
;;

let siegbert_min_winning_move heap_size =
  Sequence.range 1 heap_size ~stop:`inclusive
  |> Sequence.find_exn ~f:(fun siegbert_move ->
    not
      (can_win { heap_size = heap_size - siegbert_move; last_taken = siegbert_move }))
;;

let%expect_test "examples" =
  [ 1; 4; 17; 8; 18 ]
  |> List.map ~f:(fun heap_size -> heap_size, siegbert_min_winning_move heap_size)
  |> [%sexp_of: (int * int) list]
  |> print_s;
  [%expect {|
    ((1  1)
     (4  1)
     (17 1)
     (8  8)
     (18 5)) |}]
;;

let%expect_test "pattern" =
  List.range 1 50 ~stop:`inclusive
  |> List.map ~f:(fun heap_size -> heap_size, siegbert_min_winning_move heap_size)
  |> [%sexp_of: (int * int) list]
  |> print_s;
  [%expect
    {|
    ((1  1)
     (2  2)
     (3  3)
     (4  1)
     (5  5)
     (6  1)
     (7  2)
     (8  8)
     (9  1)
     (10 2)
     (11 3)
     (12 1)
     (13 13)
     (14 1)
     (15 2)
     (16 3)
     (17 1)
     (18 5)
     (19 1)
     (20 2)
     (21 21)
     (22 1)
     (23 2)
     (24 3)
     (25 1)
     (26 5)
     (27 1)
     (28 2)
     (29 8)
     (30 1)
     (31 2)
     (32 3)
     (33 1)
     (34 34)
     (35 1)
     (36 2)
     (37 3)
     (38 1)
     (39 5)
     (40 1)
     (41 2)
     (42 8)
     (43 1)
     (44 2)
     (45 3)
     (46 1)
     (47 13)
     (48 1)
     (49 2)
     (50 3)) |}]
;;

(* H(k) = k if k is a Fibonacci number,
   H(k - F) otherwise, where F is the largest Fibonacci number F <= k *)

let g_naive n =
  Sequence.range 1 n ~stop:`inclusive
  |> Sequence.sum (module Int) ~f:siegbert_min_winning_move
;;

let%expect_test "example" =
  g_naive 13 |> [%sexp_of: int] |> print_s;
  [%expect {| 43 |}]
;;

let%expect_test "pattern" =
  Number_theory.Int.fibonacci
  |> Fn.flip Sequence.take 20
  |> Sequence.map ~f:(fun n -> n, g_naive n)
  |> [%sexp_of: (int * int) Sequence.t]
  |> print_s;
  [%expect
    {|
    ((0    0)
     (1    1)
     (1    1)
     (2    3)
     (3    6)
     (5    12)
     (8    23)
     (13   43)
     (21   79)
     (34   143)
     (55   256)
     (89   454)
     (144  799)
     (233  1397)
     (377  2429)
     (610  4203)
     (987  7242)
     (1597 12432)
     (2584 21271)
     (4181 36287)) |}]
;;

(* For G(F(n)) where F(n) is a Fibonacci number,

   G(n) = G(F(n-2)) + G(F(n-1)) + F(n-1) *)

(* The 80th Fibonacci number *)
let ubound = 23_416_728_348_467_685

let%test _ = ubound = (Number_theory.Int.fibonacci |> Fn.flip Sequence.nth_exn 80)

let g_fibonacci =
  let fibonacci =
    lazy
      (Number_theory.Int.fibonacci
       |> Fn.flip Sequence.take 81
       |> Sequence.to_array
       |> Array.get)
  in
  Memo.recursive
    (module Int)
    (fun g_fibonacci n ->
       if n = 1 || n = 2
       then 1
       else (
         let fibonacci = force fibonacci in
         g_fibonacci (n - 2) + g_fibonacci (n - 1) + fibonacci (n - 1)))
;;

let%expect_test "debug" =
  List.range 1 20
  |> List.map ~f:(fun n ->
    n, Sequence.nth_exn Number_theory.Int.fibonacci n, g_fibonacci n)
  |> [%sexp_of: (int * int * int) list]
  |> print_s;
  [%expect
    {|
    ((1  1    1)
     (2  1    1)
     (3  2    3)
     (4  3    6)
     (5  5    12)
     (6  8    23)
     (7  13   43)
     (8  21   79)
     (9  34   143)
     (10 55   256)
     (11 89   454)
     (12 144  799)
     (13 233  1397)
     (14 377  2429)
     (15 610  4203)
     (16 987  7242)
     (17 1597 12432)
     (18 2584 21271)
     (19 4181 36287)) |}]
;;

let main () = g_fibonacci 80 |> [%sexp_of: int] |> print_s

let%expect_test "answer" =
  main ();
  [%expect {| 842043391019219959 |}]
;;

include (val Solution.make ~problem:(Number 692) ~main)
