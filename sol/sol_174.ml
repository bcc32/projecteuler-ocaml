open! Core
open! Import

(* A square lamina with outer side length a and inner side length b consists of
   [a^2 - b^2 = (a + b)(a - b)] tiles.  Thus, for a given number of tiles (t),
   the number of possible square lamina is given by the number of
   factorizations, [t = (a + b)(a - b)] where a and b are positive integers.

   For each even factor [x] of [t] where [x * x < t] and [(t/x - x) % 2 = 0], we
   can construct such a square lamina with [a - b = x] and [a + b = t / x]. *)

let count_square_laminae t =
  if t % 4 <> 0
  then 0
  else
    Number_theory.Int.divisors (t / 4)
    |> List.count ~f:(fun x ->
      let x = x * 2 in
      x * x < t)
;;

let%expect_test "examples" =
  print_s [%sexp (count_square_laminae 8 : int)];
  [%expect {| 1 |}];
  print_s [%sexp (count_square_laminae 32 : int)];
  [%expect {| 2 |}]
;;

let count_square_laminae_naive t =
  Sequence.length
    (let open Sequence.Let_syntax in
     let%bind outer =
       Number_theory.Int.natural_numbers ~init:3 ()
       |> Sequence.take_while ~f:(fun n -> (n * n) - ((n - 2) * (n - 2)) <= t)
     in
     let%bind inner = Sequence.range (outer - 2) 0 ~stride:(-2) in
     if (outer * outer) - (inner * inner) = t then return () else Sequence.empty)
;;

let%test_unit "vs naive" =
  Quickcheck.test
    Quickcheck.Generator.small_positive_int
    ~sexp_of:[%sexp_of: int]
    ~f:(fun t ->
      [%test_result: int] ~expect:(count_square_laminae_naive t) (count_square_laminae t))
;;

module M = struct
  let problem = Number 174

  let main () =
    let tiles_by_type = Hashtbl.create (module Int) in
    debug_timing
      ~here:[%here]
      (fun () ->
         for t = 1 to 1_000_000 do
           if debug && t % 10_000 = 0 then Debug.eprintf "%d" t;
           Hashtbl.add_multi tiles_by_type ~key:(count_square_laminae t) ~data:t
         done)
      ();
    if debug then Debug.eprint "done iterating";
    [%test_result: int]
      ~expect:832
      (Hashtbl.find_multi tiles_by_type 15 |> List.length)
      ~message:"Expected N(15) = 832";
    let sum = ref 0 in
    for n = 1 to 10 do
      sum := !sum + (Hashtbl.find_multi tiles_by_type n |> List.length)
    done;
    printf "%d\n" !sum
  ;;

  (* 209566
     489.617085ms *)
end

include Solution.Make (M)
