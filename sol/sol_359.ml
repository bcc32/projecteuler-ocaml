open! Core
open! Import

(* 2^27 * 3^12 *)
let large_number = 71328803586048
let floors : int Queue.t Queue.t = Queue.create ()
let num_guests = ref 0

let extend () =
  incr num_guests;
  let next_guest = !num_guests in
  let rec try_floor n =
    if n >= Queue.length floors then Queue.enqueue floors (Queue.create ());
    let floor = Queue.get floors n in
    if Queue.is_empty floor
    then Queue.enqueue floor next_guest
    else (
      let last_guest_on_floor = Queue.last_exn floor in
      if Number_theory.Int.is_perfect_square (last_guest_on_floor + next_guest)
      then Queue.enqueue floor next_guest
      else try_floor (n + 1))
  in
  try_floor 0
;;

let occupant_exn f r : int = Queue.get (Queue.get floors (f - 1)) (r - 1)

let occupied f r =
  try
    ignore (occupant_exn f r : int);
    true
  with
  | _ -> false
;;

let p f r =
  while not (occupied f r) do
    extend ()
  done;
  occupant_exn f r
;;

let%expect_test "examples" =
  [ 1, 1; 1, 2; 2, 1; 10, 20; 25, 75; 99, 100 ]
  |> List.iter ~f:(fun (f, r) -> print_s [%message (f : int) (r : int) (p f r : int)]);
  [%expect
    {|
    ((f 1) (r 1) ("p f r" 1))
    ((f 1) (r 2) ("p f r" 3))
    ((f 2) (r 1) ("p f r" 2))
    ((f 10) (r 20) ("p f r" 440))
    ((f 25) (r 75) ("p f r" 4863))
    ((f 99) (r 100) ("p f r" 19454)) |}]
;;

let%expect_test "small numbers" =
  for f = 1 to 10 do
    for r = 1 to 10 do
      printf " %3d" (p f r)
    done;
    printf "\n"
  done;
  [%expect
    {|
     1   3   6  10  15  21  28  36  45  55
     2   7   9  16  20  29  35  46  54  67
     4   5  11  14  22  27  37  44  56  65
     8  17  19  30  34  47  53  68  76  93
    12  13  23  26  38  43  57  64  80  89
    18  31  33  48  52  69  75  94 102 123
    24  25  39  42  58  63  81  88 108 117
    32  49  51  70  74  95 101 124 132 157
    40  41  59  62  82  87 109 116 140 149
    50  71  73  96 100 125 131 158 166 195 |}]
;;

let%expect_test "the squares" =
  for f = 1 to 10 do
    for r = 1 to 9 do
      printf " %3d" (p f r + p f (r + 1))
    done;
    printf "\n"
  done;
  [%expect
    {|
      4   9  16  25  36  49  64  81 100
      9  16  25  36  49  64  81 100 121
      9  16  25  36  49  64  81 100 121
     25  36  49  64  81 100 121 144 169
     25  36  49  64  81 100 121 144 169
     49  64  81 100 121 144 169 196 225
     49  64  81 100 121 144 169 196 225
     81 100 121 144 169 196 225 256 289
     81 100 121 144 169 196 225 256 289
    121 144 169 196 225 256 289 324 361 |}]
;;

(* From inspection, we determine the pattern to be:

   - The first resident of each floor increments by 1, 2, 4, 4, 6, 6, 8, 8, 10,
     10, 12, 12, ...
   - Each floor's squares starts with 2^2, 3^2, 3^2, 5^2, 5^2, 7^2, 7^2, ...
   - Each floor's squares are consecutive.

   To calculate the first resident of each floor, we can use the formula:

   {v
     p(f, 1) =
       1 if f = 1
       2 if f = 2
       4 if f = 3
       2n^2 + 4n + 2 if f is even, where n = floor(f / 2) - 1
       2n^2 + 6n + 4 if f is even, where n = floor(f / 2) - 1
   v}

   To calculate the rth entry on floor f, letting p(f, 1) be the first resident
   of that floor and s(f, 1) be the base of the first perfect square on that
   floor, we solve:

   p(f, 1) + p(f, 2) = s(f, 1)
   p(f, 2) + p(f, 3) = s(f, 2)
   p(f, 3) + p(f, 4) = s(f, 3)
   ...

   {v
     p(f, n) = s(f, n) - s(f, n-1) + s(f, n-2) - s(f, n-3) + ... + (-1)^(n-1) s(f, 1)
             + (-1)^n p(f, 1)
   v}

   The sum of alternating squares, -1^2+2^2-3^2+...+(-1)^n n^2 = (-1)^n n(n+1)/2.

   (The above can be shown by induction).

   If we fix the last term as positive, we can write the sum as simply n(n+1)/2.
*)

(* TODO: Add utility functor for this. *)
module Int_with_modulus = struct
  type t = int

  let modulus = 100_000_000
  let zero = 0
  let force x = x mod modulus
  let ( + ) a b = force (a + b)
  let ( * ) a b = force (force a * force b)
end

(** [to_^2 - (to_-1)^2 + ... down to 1].  That is, the sign of [to_^2] is
    positive, but the sign of [1^2] may be negative. *)
let sum_of_alternating_squares_from_1 ~upto =
  let open Int_with_modulus in
  upto * (upto + 1) / 2
;;

(** [to_^2 - (to_-1)^2 + ... down to from].  That is, the sign of [to_^2] is
    positive, but the sign of [from^2] may be negative. *)
let sum_of_alternating_square_range ~from ~upto =
  let open Int_with_modulus in
  let sign_of_smaller_range = if (upto - from) % 2 = 0 then 1 else -1 in
  sum_of_alternating_squares_from_1 ~upto
  + (sign_of_smaller_range * sum_of_alternating_squares_from_1 ~upto:(from - 1))
;;

let%test_unit "sum_of_alternating_square_range" =
  let gen =
    let quickcheck_generator_int = Quickcheck.Generator.small_positive_int in
    [%quickcheck.generator: int * int]
    |> Quickcheck.Generator.filter ~f:(fun (x, y) -> x <= y)
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: int * int] ~f:(fun (from, upto) ->
    let expect =
      Sequence.range upto from ~stop:`inclusive ~stride:(-1)
      |> Sequence.mapi ~f:(fun i x -> if i % 2 = 0 then x * x else -x * x)
      |> Sequence.sum (module Int) ~f:Fn.id
    in
    [%test_result: int] ~expect (sum_of_alternating_square_range ~from ~upto))
;;

let p_by_pattern f r =
  let first_resident_on_floor =
    if f = 1
    then 1
    else if f = 2
    then 2
    else if f = 3
    then 4
    else (
      let n = (f / 2) - 1 in
      if f % 2 = 0
      then Int_with_modulus.((2 * n * n) + (4 * n) + 2)
      else Int_with_modulus.((2 * n * n) + (6 * n) + 4))
  in
  if r = 1
  then first_resident_on_floor
  else (
    let first_square = if f = 1 then 2 else (f / 2 * 2) + 1 in
    let upto = first_square + r - 2 in
    let sign_of_first_resident = if r % 2 = 0 then -1 else 1 in
    let open Int_with_modulus in
    sum_of_alternating_square_range ~from:first_square ~upto
    + (sign_of_first_resident * first_resident_on_floor))
;;

let%expect_test "examples" =
  [ 1, 1; 1, 2; 2, 1; 10, 20; 25, 75; 99, 100 ]
  |> List.iter ~f:(fun (f, r) ->
    print_s [%message (f : int) (r : int) (p_by_pattern f r : int)]);
  [%expect
    {|
    ((f 1) (r 1) ("p_by_pattern f r" 1))
    ((f 1) (r 2) ("p_by_pattern f r" 3))
    ((f 2) (r 1) ("p_by_pattern f r" 2))
    ((f 10) (r 20) ("p_by_pattern f r" 440))
    ((f 25) (r 75) ("p_by_pattern f r" 4863))
    ((f 99) (r 100) ("p_by_pattern f r" 19454)) |}]
;;

(* https://projecteuler.net/thread=359#43305 *)
let p_simple_by_leonid f r =
  let open Int_with_modulus in
  if f = 1
  then r * (r + 1) / 2
  else ((r - 1) * r / 2) + (f / 2 * ((f / 2) + r - ((r + f) % 2)) * 2)
;;

let%expect_test "examples" =
  [ 1, 1; 1, 2; 2, 1; 10, 20; 25, 75; 99, 100 ]
  |> List.iter ~f:(fun (f, r) ->
    print_s [%message (f : int) (r : int) (p_simple_by_leonid f r : int)]);
  [%expect
    {|
    ((f 1) (r 1) ("p_simple_by_leonid f r" 1))
    ((f 1) (r 2) ("p_simple_by_leonid f r" 3))
    ((f 2) (r 1) ("p_simple_by_leonid f r" 2))
    ((f 10) (r 20) ("p_simple_by_leonid f r" 440))
    ((f 25) (r 75) ("p_simple_by_leonid f r" 4863))
    ((f 99) (r 100) ("p_simple_by_leonid f r" 19454)) |}]
;;

let main () =
  Number_theory.Int.divisors large_number
  |> List.sum
       (module Int_with_modulus)
       ~f:(fun f ->
         let r = large_number / f in
         p_simple_by_leonid f r)
  |> printf "%d\n"
;;

(* 34.07us *)
let%expect_test "answer" =
  main ();
  [%expect {| 40632119 |}]
;;

include (val Solution.make ~problem:(Number 359) ~main)
