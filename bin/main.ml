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

let list_solutions () =
  List.iter solution_commands ~f:(fun (_, info) -> print_endline (Term.name info))
;;

let list_solutions_t =
  Term.(const list_solutions $ const (), info "list" ~doc:"List solution commands")
;;

let main =
  Term.(
    ( ret (const (`Help (`Auto, None)))
    , info "euler" ~doc:"Run ProjectEuler solutions" ~version:"%%VERSION%%" ))
;;

let () =
  Term.(
    eval_choice main (Euler_benchmark.command :: list_solutions_t :: solution_commands)
    |> exit)
;;
