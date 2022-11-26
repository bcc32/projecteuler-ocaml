open! Core
open! Import

(* TODO: Allow positional arg to pick just one, maybe using a regexp? *)
let list_solutions verbose =
  List.iter Euler_solutions.all ~f:(fun (name, (module Sol : Solution.S)) ->
    print_endline name;
    if verbose then print_endline ("    " ^ Sol.description))
;;

let verbose_arg =
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc:"Show solution descriptions")
;;

let list_command =
  Cmd.v
    (Cmd.info "list" ~doc:"List solution commands")
    Term.(const list_solutions $ verbose_arg)
;;

let debug_arg =
  Arg.(
    value
    & flag
    & info
        ~env:(Cmd.Env.info "EULER_DEBUG")
        [ "d"; "debug" ]
        ~doc:"Enable debug/progress printing")
;;

let time_arg = Arg.(value & flag & info [ "t"; "time" ] ~doc:"Measure and print runtime")
let name_arg = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME")

let run_solution name debug time =
  match List.Assoc.find Euler_solutions.all name ~equal:String.equal with
  | None -> error_s [%message "No such solution found" (name : string)]
  | Some (module Sol) ->
    (match Sol.run ~print_debug:debug ~print_elapsed_time:time () with
     | () -> Ok ()
     | exception e -> error_s [%message "Solution raised" (name : string) ~_:(e : exn)])
;;

let run_command =
  Cmd.v
    (Cmd.info "run" ~doc:"Run a solution (by name)")
    Term.(term_result (const run_solution $ name_arg $ debug_arg $ time_arg))
;;
