open Core.Std
open Core_bench.Std

let divisors_group =
  let args = [ 17 ; 60 ; 100003 ; 120000 ; 600851475143 ] in
  Bench.Test.create_indexed
    ~name:"Euler.Int.divisors"
    ~args
    (fun n -> stage (fun () -> Euler.Int.divisors n))

let sqrt_group =
  let sqrt_newton x =
    Euler.Float.newton's_method
      ~f:(fun y -> Float.(y * y - x))
      ~f':(Float.( * ) 2.0)
      ~epsilon:1e-12
      ~init:1.0
  in
  let sqrt_bisect x =
    Euler.Float.bisect
      ~f:(fun y -> Float.(y * y - x))
      ~epsilon:1e-12
      ~low:1.0
      ~high:x
  in
  let nums = [ 1.0; 1.5; 2.0; 3.0; 100.0; 20_000.0 ] in
  let methods =
    [ sqrt_newton, "Newton's method"
    ; sqrt_bisect, "bisection"
    ; sqrt       , "built-in [sqrt] function"
    ]
  in
  Bench.Test.(
    List.map methods ~f:(fun (f, name) ->
      List.map nums ~f:(fun x ->
        create ~name:(Float.to_string x) (fun () -> f x))
      |> create_group ~name)
    |> create_group ~name:"sqrt"
  )

let command =
  let groups =
    [ "divisors", divisors_group
    ; "sqrt"    , sqrt_group
    ]
  in
  List.map groups ~f:(Tuple2.map_snd ~f:(fun g -> Bench.make_command [ g ]))
  |> Command.group ~summary:"Benchmarking Euler"
