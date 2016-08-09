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

let command =
  Bench.make_command
    [ divisors_group
    ]
