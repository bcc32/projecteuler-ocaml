open! Core
open! Import
open Core_bench.Std
open Euler

let run_length_encode_group =
  Bench.Test.create_with_initialization ~name:"Euler.run_length_encode" (fun `init ->
    let length = 200 in
    let list =
      List.gen_with_length length [%quickcheck.generator: bool]
      |> Quickcheck.random_value
    in
    fun () -> Sequences.run_length_encode list ~equal:Bool.equal)
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
    Numerics.Float.bisect ~f:(fun y -> Float.((y * y) - x)) ~epsilon:1e-12 ~lo:1.0 ~hi:x
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
  Bench.Test.create ~name:"prime_sieve(10^6)" (fun () ->
    Number_theory.prime_sieve 1_000_000)
;;

let pow_group =
  let bench_exponents f ~name =
    Bench.Test.create_indexed
      ~name
      ~args:[ 0; 1; 2; 4; 5; 10; 15; 20; 25; 30 ]
      (fun exp -> stage (fun () -> f 3 exp))
  in
  let bench_constant_exponent f ~name = Bench.Test.create ~name (fun () -> f 3 5) in
  Bench.Test.create_group
    ~name:"integer exponentiation"
    [ bench_exponents ~name:"Int.pow" Int.pow
    ; bench_exponents
        ~name:"pow_fast without functor"
        Number_theory.Int.addition_chain_pow
    ; bench_constant_exponent ~name:"Int.pow const(5)" Int.pow
    ; bench_constant_exponent
        ~name:"pow_fast without functor const(5)"
        Number_theory.Int.addition_chain_pow
    ]
;;

let command =
  let groups =
    [ "divisors", divisors_group
    ; "sqrt", sqrt_group
    ; "pow", pow_group
    ; "primes", primes_group
    ; "rle", run_length_encode_group
    ]
  in
  List.map groups ~f:(Tuple2.map_snd ~f:(fun g -> Bench.make_command [ g ]))
  |> Command.group ~summary:"Benchmarking Euler"
;;

let command =
  Cmdliner.Term.(
    ( const (fun c -> Command.run c) $ const command
    , info "bench" ~doc:"Benchmarking Euler" ))
;;
