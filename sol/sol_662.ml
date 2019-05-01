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
;;

let%expect_test "moves" =
  Expect_test_helpers_kernel.print_s [%sexp (moves : (int * int) list)];
  [%expect
    {|
    ((0     1)
     (1     0)
     (0     2)
     (2     0)
     (0     3)
     (3     0)
     (0     5)
     (5     0)
     (0     8)
     (8     0)
     (0     13)
     (13    0)
     (0     21)
     (21    0)
     (0     34)
     (34    0)
     (0     55)
     (55    0)
     (0     89)
     (89    0)
     (0     144)
     (144   0)
     (0     233)
     (233   0)
     (0     377)
     (377   0)
     (0     610)
     (610   0)
     (0     987)
     (987   0)
     (0     1597)
     (1597  0)
     (0     2584)
     (2584  0)
     (0     4181)
     (4181  0)
     (0     6765)
     (6765  0)
     (0     10946)
     (10946 0)
     (0     17711)
     (17711 0)
     (10370 3504)
     (3504  10370)
     (9790  4896)
     (4896  9790)
     (1869  3740)
     (3740  1869)
     (819   4100)
     (4100  819)
     (715   1428)
     (1428  715)
     (345   152)
     (152   345)
     (546   272)
     (272   546)
     (135   352)
     (352   135)
     (414   448)
     (448   414)
     (754   10920)
     (10920 754)
     (6171  2772)
     (2772  6171)
     (105   208)
     (208   105)
     (4389  5148)
     (5148  4389)
     (555   4144)
     (4144  555)
     (7761  15920)
     (15920 7761)
     (39    80)
     (80    39)
     (110   600)
     (600   110)
     (3955  1356)
     (1356  3955)
     (1485  6600)
     (6600  1485)
     (273   260)
     (260   273)
     (2280  1216)
     (1216  2280)
     (30    16)
     (16    30)
     (4210  10104)
     (10104 4210)
     (145   348)
     (348   145)
     (5     12)
     (12    5)
     (4059  5412)
     (5412  4059)
     (366   488)
     (488   366)
     (33    44)
     (44    33)
     (3     4)
     (4     3)) |}]
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
     4m16.056538322s *)
end

include Solution.Make (M)
