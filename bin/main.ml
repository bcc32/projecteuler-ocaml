open! Core

let solution_commands =
  Euler_solutions.modules
  |> List.map ~f:(Tuple2.map_snd ~f:(fun m ->
    let module M = (val m : Euler.Solution_intf.S) in M.command))
;;

let command =
  let commands = solution_commands @ ["bench", Benchmark.command] in
  Command.group commands
    ~summary:"Run ProjectEuler solutions"
    ~preserve_subcommand_order:()
;;

let () = Command.run ~version:"0.2.0" command
