open! Core
open! Import

(* See https://euler.stephan-brumme.com/611/ and https://en.wikipedia.org/wiki/Fermat%27s_theorem_on_sums_of_two_squares
*)

let f_naive n =
  let opened = Hashtbl.create (module Int) in
  let rec loop1 i ii =
    if ii > n
    then ()
    else (
      let rec loop2 j jj =
        if ii + jj > n
        then ()
        else (
          Hashtbl.incr opened (ii + jj);
          let j = j + 1 in
          loop2 j (j * j))
      in
      let i = i + 1 in
      loop2 i (i * i);
      loop1 i (i * i))
  in
  loop1 1 1;
  Hashtbl.filter_inplace opened ~f:(fun times -> times mod 2 <> 0);
  let is_open = Hash_set.of_hashtbl_keys opened in
  Hash_set.length is_open
;;

let%expect_test "examples" =
  print_s [%message (f_naive 5 : int) (f_naive 100 : int) (f_naive 1_000 : int)];
  [%expect {| (("f_naive 5" 1) ("f_naive 100" 27) ("f_naive 1_000" 233)) |}]
;;

let f n =
  Number_theory.Int.primes
  |> Sequence.filter ~f:(fun p -> p mod 4 = 1)
  |> Sequence.take_while ~f:(fun p -> p <= n)
  |> Sequence.sum (module Int) ~f:(fun p -> Number_theory.Int.isqrt (n / p))
;;

let%expect_test "examples" =
  Expect_test_helpers_kernel.show_raise ~hide_positions:true (fun () ->
    print_s [%message (f 5 : int) (f 100 : int) (f 1_000 : int)]);
  [%expect {|
    (("f 5" 1) ("f 100" 16) ("f 1_000" 144))
    "did not raise" |}]
;;

let main () = printf "%d\n" @@ f_naive 1_000_000

(* incomplete *)
include (val Solution.make ~problem:(Number 611) ~main)
