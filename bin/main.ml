open! Core
open! Import

let solution_commands =
  Euler_solutions.all
  |> List.map ~f:(fun m ->
    let module M = (val m : Solution.S) in
    (* FIXME Work around automatic prefix matching, since we don't want [euler
       60] to run [euler 601] if no solution to problem 60 is written. *)
    (* TODO: Use cmdliner?  Would also take care of the EULER_DEBUG environment
       variable. *)
    M.command_name, M.command)
;;

let command =
  let commands = solution_commands @ [ "bench", Benchmark.command ] in
  Command.group
    commands
    ~summary:"Run ProjectEuler solutions"
    ~preserve_subcommand_order:()
;;

let () = Command.run ~version:"%%VERSION%%" command
