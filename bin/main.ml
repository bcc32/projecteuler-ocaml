open! Core
open! Import

(* FIXME Work around automatic prefix matching, since we don't want [euler
   60] to run [euler 601] if no solution to problem 60 is written. *)
let solution_commands =
  Euler_solutions.all
  |> List.map ~f:(fun m ->
    let module M = (val m : Solution.S) in
    M.command)
;;

let main =
  Term.(
    ( ret (const (`Help (`Pager, None)))
    , info "euler" ~doc:"Run ProjectEuler solutions" ~version:"%%VERSION%%" ))
;;

let () = Term.(exit @@ eval_choice main (Benchmark.command :: solution_commands))
