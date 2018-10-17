open! Core
open! Import

let solution_commands =
  Euler_solutions.all
  |> List.map ~f:(fun m ->
    let module M = (val m : Solution.S) in
    M.command_name, M.command)
;;

let command =
  let commands = solution_commands @ [ "bench", Benchmark.command ] in
  Command.group
    commands
    ~summary:"Run ProjectEuler solutions"
    ~preserve_subcommand_order:()
;;

let () = Command.run ~version:"0.2.0" command
