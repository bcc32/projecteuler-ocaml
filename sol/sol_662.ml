open! Core
open! Import

let grid_length = 10_000
let grid_diagonal = Float.iround_down_exn (sqrt 2. *. float grid_length)
let modulus = 1_000_000_007

let fibonacci =
  Number_theory.Int.fibonacci
  |> Fn.flip Sequence.drop 2
  |> Sequence.take_while ~f:(fun x -> x <= grid_diagonal)
  |> Sequence.to_list
;;

let%expect_test "fibonacci" =
  print_s [%sexp (fibonacci : int list)];
  [%expect
    {| (1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946) |}]
;;

let moves =
  let hypotenuses =
    let is_fibonacci =
      let set = Set.of_list (module Int) fibonacci in
      Set.mem set
    in
    let triangles = ref [] in
    Geometry.iter_all_pythagorean_triples
      ~with_hypotenuse_less_than:grid_diagonal
      ~f:(fun a b c -> if is_fibonacci c then triangles := (a, b) :: !triangles);
    !triangles
  in
  List.map fibonacci ~f:(fun n -> 0, n) @ hypotenuses
  |> List.concat_map ~f:(fun (x, y) ->
    (* We can guarantee that [x <> y] because [x = y] can't be the legs of a right
       triangle. *)
    assert (x <> y);
    [ x, y; y, x ])
  |> List.filter ~f:(fun (x, y) -> x <= grid_length && y <= grid_length)
  |> List.sort ~compare:(Comparable.lift Int.compare ~f:(fun (x, y) -> x + y))
;;

let%expect_test "moves" =
  Expect_test_helpers_kernel.print_s [%sexp (moves : (int * int) list)];
  [%expect
    {|
    ((0    1)
     (1    0)
     (0    2)
     (2    0)
     (0    3)
     (3    0)
     (0    5)
     (5    0)
     (3    4)
     (4    3)
     (0    8)
     (8    0)
     (0    13)
     (13   0)
     (5    12)
     (12   5)
     (0    21)
     (21   0)
     (0    34)
     (34   0)
     (30   16)
     (16   30)
     (0    55)
     (55   0)
     (33   44)
     (44   33)
     (0    89)
     (89   0)
     (39   80)
     (80   39)
     (0    144)
     (144  0)
     (0    233)
     (233  0)
     (105  208)
     (208  105)
     (0    377)
     (377  0)
     (135  352)
     (352  135)
     (145  348)
     (348  145)
     (345  152)
     (152  345)
     (273  260)
     (260  273)
     (0    610)
     (610  0)
     (110  600)
     (600  110)
     (546  272)
     (272  546)
     (366  488)
     (488  366)
     (414  448)
     (448  414)
     (0    987)
     (987  0)
     (0    1597)
     (1597 0)
     (715  1428)
     (1428 715)
     (0    2584)
     (2584 0)
     (2280 1216)
     (1216 2280)
     (0    4181)
     (4181 0)
     (555  4144)
     (4144 555)
     (819  4100)
     (4100 819)
     (3955 1356)
     (1356 3955)
     (1869 3740)
     (3740 1869)
     (0    6765)
     (6765 0)
     (1485 6600)
     (6600 1485)
     (6171 2772)
     (2772 6171)
     (4059 5412)
     (5412 4059)
     (4389 5148)
     (5148 4389)
     (9790 4896)
     (4896 9790)) |}]
;;

let ways w h =
  let w, h = if w < h then w, h else h, w in
  let cache = Array.create 0 ~len:((w + 1) * (h + 1)) in
  let index x y = ((h + 1) * x) + y in
  let get x y = cache.(index x y) in
  cache.(0) <- 1;
  for x = 0 to w do
    for y = x to h do
      if x > 0 || y > 0
      then (
        let ways_x_y =
          List.sum
            (module Int)
            moves
            ~f:(fun (dx, dy) -> if x >= dx && y >= dy then get (x - dx) (y - dy) else 0)
          mod modulus
        in
        cache.(index x y) <- ways_x_y;
        if y <= w then cache.(index y x) <- ways_x_y)
    done
  done;
  get w h
;;

let%expect_test "F(3, 4)" =
  print_s [%sexp (ways 3 4 : int)];
  [%expect {| 278 |}]
;;

let%expect_test "F(10, 10)" =
  print_s [%sexp (ways 10 10 : int)];
  [%expect {| 215846462 |}]
;;

let problem = Number 662

let main () =
  let ans = ways grid_length grid_length in
  print_s [%sexp (ans : int)]
;;

(* 860873428
   50.213826006s *)

include (val Solution.make ~problem ~main)
