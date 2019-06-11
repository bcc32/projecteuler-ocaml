open! Core
open! Import

let prime_factors =
  Memo.general ~hashable:Int.hashable (fun n ->
    Number_theory.Int.factor n |> List.dedup_and_sort ~compare:Int.compare)
;;

let prime_factors_product =
  Memo.general ~hashable:Int.hashable (fun n ->
    prime_factors n |> List.fold ~init:1 ~f:( * ))
;;

(* precondition: a, b, c are pairwise coprime *)
let radical a b c =
  prime_factors_product a * prime_factors_product b * prime_factors_product c
;;

let%expect_test _ =
  print_s [%sexp (radical 8 9 7 : int)];
  [%expect {| 42 |}]
;;

let sieve ubound pfs =
  let is_coprime = Array.create true ~len:(ubound + 1) in
  List.iter pfs ~f:(fun pf ->
    for i = 0 to ubound / pf do
      is_coprime.(i * pf) <- false
    done);
  Array.filter_mapi is_coprime ~f:(fun i x -> if x then Some i else None)
;;

let abc_hits ubound =
  Sequence.range 1 ubound
  |> Sequence.sum
       (module Int)
       ~f:(fun c ->
         if debug && c mod 100 = 0 then Debug.eprintf "c=%d" c;
         let pfs = prime_factors c in
         c
         * (sieve ((c / 2) - 1) pfs
            |> Array.count ~f:(fun a ->
              let b = c - a in
              radical a b c < c)))
;;

let%expect_test _ =
  print_s [%sexp (abc_hits 1000 : int)];
  [%expect {| 12523 |}]
;;

let main () = abc_hits 120_000 |> printf "%d\n"

(* 18407904
   11.8236m *)

include (val Solution.make ~problem:(Number 127) ~main)
