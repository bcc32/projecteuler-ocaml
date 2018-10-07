open! Core
open! Import

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
      | Number n -> sprintf "Problem %d" n
      | Custom { number; tag = _; description } ->
        sprintf "Problem %d (%s)" number description
    in
    Command.basic main ~summary
  ;;

  let command_name =
    match M.problem with
    | Number n -> Int.to_string n
    | Custom { number; tag; description = _ } -> sprintf "%d-%s" number tag
  ;;
end
