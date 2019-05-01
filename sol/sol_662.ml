open! Core
open! Import

let grid_length = 10_000

let fibonacci =
  Number_theory.Int.fibonacci
  |> Fn.flip Sequence.drop 2
  |> Sequence.take_while ~f:(fun x -> x <= 2 * grid_length)
  |> Sequence.to_list
;;

let%expect_test "fibonacci" =
  print_s [%sexp (fibonacci : int list)];
  [%expect
    {|
      (1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946
       17711) |}]
;;

let moves =
  let hypotenuses =
    let is_fibonacci =
      let set = Int.Set.of_list fibonacci in
      Set.mem set
    in
    let triangles = ref [] in
    Geometry.iter_all_pythagorean_triples
      ~with_hypotenuse_less_than:(2 * grid_length)
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
  |> List.sort ~compare:[%compare: int * int]
;;

let%expect_test "moves" =
  Expect_test_helpers_kernel.print_s [%sexp (moves : (int * int) list)];
  [%expect
    {|
    ((0    1)
     (0    2)
     (0    3)
     (0    5)
     (0    8)
     (0    13)
     (0    21)
     (0    34)
     (0    55)
     (0    89)
     (0    144)
     (0    233)
     (0    377)
     (0    610)
     (0    987)
     (0    1597)
     (0    2584)
     (0    4181)
     (0    6765)
     (1    0)
     (2    0)
     (3    0)
     (3    4)
     (4    3)
     (5    0)
     (5    12)
     (8    0)
     (12   5)
     (13   0)
     (16   30)
     (21   0)
     (30   16)
     (33   44)
     (34   0)
     (39   80)
     (44   33)
     (55   0)
     (80   39)
     (89   0)
     (105  208)
     (110  600)
     (135  352)
     (144  0)
     (145  348)
     (152  345)
     (208  105)
     (233  0)
     (260  273)
     (272  546)
     (273  260)
     (345  152)
     (348  145)
     (352  135)
     (366  488)
     (377  0)
     (414  448)
     (448  414)
     (488  366)
     (546  272)
     (555  4144)
     (600  110)
     (610  0)
     (715  1428)
     (819  4100)
     (987  0)
     (1216 2280)
     (1356 3955)
     (1428 715)
     (1485 6600)
     (1597 0)
     (1869 3740)
     (2280 1216)
     (2584 0)
     (2772 6171)
     (3740 1869)
     (3955 1356)
     (4059 5412)
     (4100 819)
     (4144 555)
     (4181 0)
     (4389 5148)
     (4896 9790)
     (5148 4389)
     (5412 4059)
     (6171 2772)
     (6600 1485)
     (6765 0)
     (9790 4896)) |}]
;;

module Int_with_modulus = struct
  type t = int

  let zero = 0
  let modulus = 1_000_000_007
  let ( + ) x y = (x + y) mod modulus
end

let rec ways =
  let cache = Array.create (-1) ~len:((grid_length + 1) * (grid_length + 1)) in
  let findi_or_add w h ~default =
    let index = (grid_length * w) + h in
    let cached = cache.(index) in
    if cached = -1
    then (
      let x = default w h in
      cache.(index) <- x;
      x)
    else cached
  in
  fun w h ->
    if w < 0 || h < 0
    then 0
    else if w = 0 && h = 0
    then 1
    else if w > h
    then ways h w
    else
      findi_or_add w h ~default:(fun w h ->
        if debug then Debug.eprint_s [%message (w : int) (h : int)];
        List.sum
          (module Int_with_modulus)
          moves
          ~f:(fun (dx, dy) -> ways (w - dx) (h - dy)))
;;

let%expect_test "F(3, 4)" =
  print_s [%sexp (ways 3 4 : int)];
  [%expect {| 278 |}]
;;

let%expect_test "F(10, 10)" =
  print_s [%sexp (ways 10 10 : int)];
  [%expect {| 215846462 |}]
;;

module M = struct
  let problem = Number 662

  let main () =
    let ans = ways grid_length grid_length in
    print_s [%sexp (ans : int)]
  ;;

  (* 860873428
     3m26.693256358s *)
end

include Solution.Make (M)
