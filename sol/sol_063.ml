open! Core
open! Import

let number_of_digits_in_pow a b =
  1 + (float b *. Float.log10 (float a) |> Float.iround_down_exn)
;;

let%test_unit _ =
  let pos = Quickcheck.Generator.small_positive_int in
  Quickcheck.test
    (Quickcheck.Generator.tuple2 pos pos)
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (a, b) ->
      let expect =
        Bigint.pow (Bigint.of_int a) (Bigint.of_int b)
        |> Bigint.to_string
        |> String.length
      in
      [%test_result: int] (number_of_digits_in_pow a b) ~expect)
;;

(* n-digit numbers are those in the half-open interval [10^(n-1), 10^n).
   Clearly, k^n is outside of this range for k >= 10, so we only concern
   ourselves with k < 10.

   We can limit ourselves to n where k^n >= 10^(n - 1) for some k < 10.  Clearly
   this is true the longest for k=9, so we solve 9^n >= 10^(n - 1) to get:

   {v
     9^n                  >= 10^(n - 1)
     n log(9)             >= (n - 1) log(10)
     (log(10) - log(9)) n <= log(10)
     n                    <= 21.8543453284
   v}
*)

(* n-digit numbers that are also nth powers *)
let count_digit_powers n =
  List.range 1 10 |> List.count ~f:(fun x -> number_of_digits_in_pow x n = n)
;;

let main () =
  List.range 1 22 |> List.sum (module Int) ~f:count_digit_powers |> printf "%d\n"
;;

(* 49
   0.046ms *)

include (val Solution.make ~problem:(Number 63) ~main)
