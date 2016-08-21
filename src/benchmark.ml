open Core.Std
open Core_bench.Std

let divisors_group =
  let open Bench.Test in
  let divisors = Euler.Int.divisors in
  create_group ~name:"Euler.divisors"
    [ create ~name:"small prime: 17" (fun () ->
        divisors 17)
    ; create ~name:"small composite: 60" (fun () ->
        divisors 60)
    ; create ~name:"ProjectEuler problem 3: 600851475143" (fun () ->
        divisors 600851475143)
    ]

let sqrt_group =
  let open Bench.Test in
  let sqrt_newton x =
    Euler.Float.newton's_method
      ~f:(fun y -> Float.(y * y - x))
      ~f':(Float.( * ) 2.0)
      ~epsilon:1e-6
      ~init:1.0
  in
  let sqrt_bisect x =
    Euler.Float.bisect
      ~f:(fun y -> Float.(y * y - x))
      ~epsilon:1e-6
      ~low:1.0
      ~high:x
  in
  let nums = [ 1.0; 1.5; 2.0; 3.0; 100.0; 20_000.0 ] in
  let methods =
    [ sqrt_newton, "Newton's method"
    ; sqrt_bisect, "bisection"
    ]
  in
  List.map methods ~f:(fun (f, name) ->
    List.map nums ~f:(fun x ->
      create ~name:(Float.to_string x) (fun () -> f x))
    |> create_group ~name)
  |>
  create_group ~name:"sqrt"

let command =
  let groups =
    [ "divisors", divisors_group
    ; "sqrt"    , sqrt_group
    ]
  in
  List.map groups ~f:(Tuple2.map_snd ~f:(fun g -> Bench.make_command [ g ]))
  |> Command.group ~summary:"Benchmarking Euler"
