open! Core
open! Import

(* Compare [p^q q^p] and [limit^limit] *)
let compare_powers ~p ~q ~limit =
  Float.compare ((p *. Float.log q) +. (q *. Float.log p)) (limit *. Float.log limit)
;;

(* Return the number of distinct integers [n = p^q q^p] where [p != q] are prime, and
   where [n <= limit^limit].

   Assume WLOG that [p < q].

   The largest [q] we need to search will be the one for which [p = 2] causes [n] to
   exceed [limit^limit].

   2^q q^2 <= limit^limit

   q log(2) + 2 log(q) <= limit log(limit)

   q log(2) <= limit log(limit)

   q <= limit log(limit) / log(2) *)
let count_hybrids limit =
  let limit = float limit in
  let max_q = Float.iround_down_exn (limit *. Float.log limit /. Float.log 2.) in
  let primes =
    Number_theory.prime_sieve max_q
    |> Array.filter_mapi ~f:(fun i b -> Option.some_if b (float i))
  in
  Array.fold primes ~init:0 ~f:(fun count q ->
    Array.fold_until
      primes
      ~init:count
      ~f:(fun count p ->
        if Float.( >= ) p q
        then Stop count
        else if compare_powers ~p ~q ~limit > 0
        then Stop count
        else Continue (count + 1))
      ~finish:Fn.id)
;;

let main () =
  let ans = count_hybrids 800_800 in
  print_s [%sexp (ans : int)]
;;

(* 26.183500358s *)
let%expect_test "answer" =
  main ();
  [%expect {| 1412403576 |}]
;;

include (val Solution.make ~problem:(Number 800) ~main)
