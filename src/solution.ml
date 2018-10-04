open! Core

let time_unit f () =
  let start = Time_ns.now () in
  protect ~f ~finally:(fun () ->
    let finish = Time_ns.now () in
    print_s [%sexp (Time_ns.diff finish start : Time_ns.Span.t)])
;;

module Make (M : Solution_intf.Arg) = struct
  let command =
    let main =
      let open Command.Let_syntax in
      let%map_open time = flag "-time" no_arg ~doc:" measure and print runtime" in
      fun () -> if time then time_unit M.main () else M.main ()
    in
    let summary =
      match M.problem with
      | `Number n -> sprintf "Problem %d" n
      | `Custom (n, `Tag _, `Description d) -> sprintf "Problem %d (%s)" n d
    in
    Command.basic main ~summary
  ;;

  let command_name =
    match M.problem with
    | `Number n -> Int.to_string n
    | `Custom (n, `Tag k, `Description _) -> sprintf "%d-%s" n k
  ;;
end
