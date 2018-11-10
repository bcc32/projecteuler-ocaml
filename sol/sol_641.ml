open! Core
open! Import

let count_prime_power_combinations n ~primes ~allowable_exponents =
  let log_ubound = Float.log n in
  let rec loop_over_primes ~primes ~allowable_exponents ~nd_mod_6 ~log_n acc =
    match primes with
    | [] -> acc + if nd_mod_6 = 1 then 1 else 0
    | prime :: primes ->
      let rec loop_over_exponents ~exponents ~nd_mod_6 ~log_n acc =
        match exponents with
        | [] -> acc
        | expt :: exponents ->
          if debug then Debug.eprint_s [%message "" (prime : int) (expt : int)];
          let new_log_n = Float.(log_n + (of_int expt * log (of_int prime))) in
          if Float.( <= ) new_log_n log_ubound
          then (
            let acc =
              loop_over_primes
                acc
                ~primes
                ~allowable_exponents
                ~nd_mod_6:(nd_mod_6 * (expt + 1) % 6)
                ~log_n:new_log_n
            in
            loop_over_exponents ~exponents ~nd_mod_6 ~log_n acc)
          else acc
      in
      loop_over_exponents ~exponents:allowable_exponents ~nd_mod_6 ~log_n acc
  in
  loop_over_primes ~primes ~allowable_exponents ~nd_mod_6:1 ~log_n:0. 0
;;

let f n =
  (* a such that (a + 1) is coprime to 6 *)
  let allowable_exponents =
    (* maximum exponent is when base is 2 *)
    let ubound = Float.(log n / log 2. |> iround_down_exn) in
    List.range 0 ubound ~stop:`inclusive
    |> List.filter ~f:(fun a -> (a + 1) % 2 <> 0 && (a + 1) % 3 <> 0)
  in
  if debug then Debug.eprint_s [%message "" (allowable_exponents : int list)];
  let primes =
    let prime_ubound =
      Float.(max ((n / 16.) ** 0.25) (n ** (1. / 6.)) |> iround_down_exn)
    in
    Number_theory.prime_sieve prime_ubound
    |> Array.filter_mapi ~f:(fun i p -> Option.some_if p i)
    |> Array.to_list
  in
  if debug then Debug.eprint_s [%message "" (primes : int list)];
  count_prime_power_combinations n ~primes ~allowable_exponents
;;

let%expect_test "f(100)" =
  print_s [%sexp (f 100. : int)];
  [%expect {| 2 |}]
;;

let%expect_test "f(10^8)" =
  print_s [%sexp (f 1e8 : int)];
  [%expect {| 69 |}]
;;

let limit = 1e36
let main () = printf "%d\n" (f limit)

include (val Solution.make ~problem:(Number 641) ~main)
