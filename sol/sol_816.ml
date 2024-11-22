open! Core
open! Import

let modulus = 50_515_093
let s0 = 290_797

let points k =
  let step s = s * s % modulus in
  Sequence.unfold_step ~init:s0 ~f:(fun s2n ->
    let s2n' = step s2n in
    let s2n'' = step s2n' in
    Yield { value = s2n, s2n'; state = s2n'' })
  |> Fn.flip Sequence.take k
;;

let dist (x1, y1) (x2, y2) =
  sqrt (float ((x2 - x1) * (x2 - x1)) +. float ((y2 - y1) * (y2 - y1)))
;;

let solve_naive points =
  let best_dist = ref Float.infinity in
  let consider p1 p2 =
    let d = dist p1 p2 in
    if Float.( < ) d !best_dist then best_dist := d
  in
  let points = Sequence.to_array points in
  for i = 0 to Array.length points - 2 do
    for j = i + 1 to Array.length points - 1 do
      consider points.(i) points.(j)
    done
  done;
  !best_dist
;;

let example () = solve_naive (points 14) |> printf "%.9f\n"

let%expect_test "example" =
  example ();
  [%expect {| 546446.466846479 |}]
;;

(* We partition the space into a grid of roughly-evenly sized buckets.

   Let z be the bucket "size", where each bucket (m, n) contains the points (x, y)
   s.t. $nz <= x < (n+1)z$ and $mz <= y < (m+1)z$.

   Assuming that the answer is less than z, we only need to check the distances between
   points in the same bucket or adjacent buckets, as points in farther buckets will
   necessarily have at least one of their x or y distances be greater than z, and so
   cannot be a closest pair.

   Let the number of buckets be b.  Assuming the points are distributed uniformly, if the
   bucket size is z, then the total number of buckets is b = (modulus / z)^2, and the
   number of bucket pairs (including a bucket with itself) we need to consider is about 5b
   (each bucket must be paired with itself, and the bucket to its top-left, right,
   bottom-right, and bottom).  The average number of points in each bucket should be about
   k / b, so the number of distance calculations per bucket pair is about (k / b)^2.

   The total number of distance calculations is therefore around 5b * (k / b)^2 or 5 k^2 /
   b.  Substituting, we get num_calculations ~= 5 k^2 z^2 / modulus^2.

   Based on the example, we know that z < 550_000, but we can guess a smaller number and
   check our assumption at the end for correctness. *)
let solve points z =
  let num_buckets_1d = (modulus / z) + 1 in
  let grid =
    debug_timing [%here] "allocate grid" (fun () ->
      Array.init num_buckets_1d ~f:(fun _ -> Array.init num_buckets_1d ~f:(fun _ -> [])))
  in
  debug_timing [%here] "insert points into grid" (fun () ->
    Sequence.iter points ~f:(fun (x, y) ->
      let grid_x = x / z in
      let grid_y = y / z in
      grid.(grid_x).(grid_y) <- (x, y) :: grid.(grid_x).(grid_y)));
  let best_dist = ref Float.infinity in
  let check_grid_points ~grid_x_1 ~grid_y_1 ~grid_x_2 ~grid_y_2 =
    let points1 = grid.(grid_x_1).(grid_y_1) in
    let points2 = grid.(grid_x_2).(grid_y_2) in
    List.iter points1 ~f:(fun p1 ->
      List.iter points2 ~f:(fun p2 ->
        let d = dist p1 p2 in
        if Float.( <> ) d 0. && Float.( < ) d !best_dist then best_dist := d))
  in
  debug_timing [%here] "check points in adjacent buckets" (fun () ->
    for grid_x_1 = 0 to num_buckets_1d - 1 do
      for grid_y_1 = 0 to num_buckets_1d - 1 do
        List.iter
          [ ( grid_x_1
            , grid_y_1
              (* We could be more clever and avoid symmetrically trying pairs within the
                 same bucket, but meh. *) )
          ; grid_x_1 + 1, grid_y_1
          ; grid_x_1 + 1, grid_y_1 + 1
          ; grid_x_1 + 1, grid_y_1 - 1
          ; grid_x_1, grid_y_1 + 1
          ]
          ~f:(fun (grid_x_2, grid_y_2) ->
            if 0 <= grid_x_2
               && grid_x_2 < num_buckets_1d
               && 0 <= grid_y_2
               && grid_y_2 < num_buckets_1d
            then check_grid_points ~grid_x_1 ~grid_y_1 ~grid_x_2 ~grid_y_2)
      done
    done);
  assert (Float.( < ) !best_dist (float z));
  !best_dist
;;

let example_with_grid () = solve (points 14) 600_000 |> printf "%.9f\n"

let%expect_test "example_with_grid" =
  example_with_grid ();
  [%expect {| 546446.466846479 |}]
;;

let main () = solve (points 2_000_000) 100_000 |> printf "%.9f\n"

(* 1.24s *)
let%expect_test "answer" =
  main ();
  [%expect {| 20.880613018 |}]
;;

include (val Solution.make ~problem:(Number 816) ~main)
