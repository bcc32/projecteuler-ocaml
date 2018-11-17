open! Core
open! Import

module M = struct
  let problem = Number 142

  (* let squares = Int.Hash_set.create () *)
  let squareq = Deque.create ()
  let i = ref 1

  let rec work ubound =
    let x = !i * !i in
    if x <= ubound
    then (
      (* Hash_set.add squares x; *)
      Deque.enqueue_back squareq x;
      incr i;
      work ubound)
  ;;

  (* let is_perfect_square = Hash_set.mem squares *)
  (* Surprisingly (or perhaps not!) checking for perfect squareness this way is
     about 20x faster than using [Number_theory.Int.is_perfect_square] and about
     3-4x faster than keeping a hash set. *)
  let is_perfect_square x =
    let y = sqrt (float x) |> Int.of_float_unchecked in
    y * y = x
  ;;

  let main () =
    let rec loop return x =
      work (2 * x);
      with_return (fun { return = break } ->
        Deque.iter squareq ~f:(fun x_y ->
          let y = x - x_y in
          if y <= 0 then break ();
          if is_perfect_square (x + y)
          then
            Deque.iter squareq ~f:(fun y_z ->
              let z = y - y_z in
              if z <= 0 then break ();
              if is_perfect_square (y + z)
              && is_perfect_square (x + z)
              && is_perfect_square (x - z)
              then return (x + y + z))));
      loop return (x + 1)
    in
    with_return (fun { return } -> loop return 1) |> printf "%d\n"
  ;;

  (* 1006193
     3.32094s *)
end

include Solution.Make (M)
