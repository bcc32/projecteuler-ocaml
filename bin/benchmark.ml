open! Core
open! Import
open Core_bench.Std
open Euler

let run_length_encode_group =
  Bench.Test.create_with_initialization ~name:"Euler.run_length_encode" (fun `init ->
    let length = 200 in
    let list =
      List.gen_with_length length Int.quickcheck_generator |> Quickcheck.random_value
    in
    fun () -> Sequences.run_length_encode Int.equal list)
;;

let divisors_group =
  let args = [ 17; 60; 100003; 120000; 600851475143 ] in
  Bench.Test.create_indexed ~name:"Euler.Int.divisors" ~args (fun n ->
    stage (fun () -> Number_theory.Int.divisors n))
;;

let sqrt_group =
  let sqrt_newton x =
    Numerics.Float.newton's_method
      ~f:(fun y -> Float.((y * y) - x))
      ~f':(Float.( * ) 2.0)
      ~epsilon:1e-12
      ~init:1.0
  in
  let sqrt_bisect x =
    Numerics.Float.bisect
      ~f:(fun y -> Float.((y * y) - x))
      ~epsilon:1e-12
      ~low:1.0
      ~high:x
  in
  let nums = [ 1.0; 1.5; 2.0; 3.0; 100.0; 20_000.0 ] in
  let methods =
    [ sqrt_newton, "Newton's method"
    ; sqrt_bisect, "bisection"
    ; sqrt, "built-in [sqrt] function"
    ]
  in
  let open Bench.Test in
  List.map methods ~f:(fun (f, name) ->
    List.map nums ~f:(fun x -> create ~name:(Float.to_string x) (fun () -> f x))
    |> create_group ~name)
  |> create_group ~name:"sqrt"
;;

let primes_group =
  let primes_sequence limit =
    Number_theory.Int.primes
    |> Sequence.take_while ~f:(fun x -> x < limit)
    |> Sequence.iter ~f:(fun x -> ignore (Sys.opaque_identity x))
  in
  let primes_sieve limit =
    ignore (Sys.opaque_identity (Number_theory.prime_sieve limit))
  in
  let limits = [ 1000; 10_000; 100_000; 1_000_000 ] in
  let methods = [ primes_sequence, "Sequence.t"; primes_sieve, "Eratosthenes' sieve" ] in
  let open Bench.Test in
  List.map methods ~f:(fun (f, name) ->
    List.map limits ~f:(fun limit ->
      create ~name:(Int.to_string limit) (fun () -> f limit))
    |> create_group ~name)
  |> create_group ~name:"primes"
;;

let command =
  let groups =
    [ "divisors", divisors_group
    ; "sqrt", sqrt_group
    ; "primes", primes_group
    ; "rle", run_length_encode_group
    ]
  in
  List.map groups ~f:(Tuple2.map_snd ~f:(fun g -> Bench.make_command [ g ]))
  |> Command.group ~summary:"Benchmarking Euler"
;;
