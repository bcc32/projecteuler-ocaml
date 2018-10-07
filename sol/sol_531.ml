open! Core
open! Import

let g a n b m =
  let open Number_theory.Int in
  let x = gcd n m in
  if a % x <> b % x
  then 0
  else (
    let y, modulus = chinese_remainder_theorem [ a / x, n / x; b / x, m / x ] in
    (y % modulus * x) + (a % x))
;;

let%expect_test "g(2, 4, 4, 6)" =
  print_s [%sexp (g 2 4 4 6 : int)];
  [%expect {| 10 |}]
;;

let%expect_test "g(3, 4, 4, 6)" =
  print_s [%sexp (g 3 4 4 6 : int)];
  [%expect {| 0 |}]
;;

let%test_unit _ =
  let limit = 50 in
  let max_result = limit * limit in
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind () = return ()
    and n = Int.gen_incl 1 limit
    and m = Int.gen_incl 1 limit in
    let%map () = return ()
    and a = Int.gen_incl 0 (n - 1)
    and b = Int.gen_incl 0 (m - 1) in
    a, n, b, m
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: int * int * int * int] ~f:(fun (a, n, b, m) ->
    let lowest =
      Sequence.range 0 max_result
      |> Sequence.find ~f:(fun x -> x % n = a && x % m = b)
      |> Option.value ~default:0
    in
    [%test_result: int] (g a n b m) ~expect:lowest)
;;

let phi =
  let cache = Int.Table.create () in
  fun n -> Hashtbl.find_or_add cache n ~default:(fun () -> Number_theory.Int.totient n)
;;

let f n m = g (phi n) n (phi m) m

module M = struct
  let problem = Number 531

  let main () =
    let sum = ref 0 in
    for n = 1_000_000 to 1_004_998 do
      for m = n + 1 to 1_004_999 do
        sum := !sum + f n m
      done
    done;
    printf "%d\n" !sum
  ;;

  (* 4515432351156203105
     10s *)
end

include Solution.Make (M)
