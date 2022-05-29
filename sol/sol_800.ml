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
  if debug then Debug.eprint_s [%message (max_q : int)];
  let primes =
    debug_timing [%here] "prime sieve" (fun () -> Number_theory.prime_sieve max_q)
    |> Array.filter_mapi ~f:(fun i b -> Option.some_if b (float i))
  in
  (* Keep track of the maximum value of [p] for each value of [q].  The max [p] for [q']
     is probably close to the max [p] for [q]. *)
  let p_index = ref (-1) in
  Array.fold primes ~init:0 ~f:(fun count q ->
    (* If [p] is bounded by [q] and not [limit], increase it. *)
    if Float.equal primes.(!p_index + 2) q then incr p_index;
    (* Bound [p] by [limit] *)
    while !p_index >= 0 && compare_powers ~p:primes.(!p_index) ~q ~limit > 0 do
      decr p_index
    done;
    count + (!p_index + 1))
;;

let main () =
  let ans = count_hybrids 800_800 in
  print_s [%sexp (ans : int)]
;;

(* 348.114382ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 1412403576 |}]
;;

include (val Solution.make ~problem:(Number 800) ~main)
