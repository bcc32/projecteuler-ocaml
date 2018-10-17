open! Core
open! Import

module type Arg = Solution_intf.Arg
module type S = Solution_intf.S

let time_unit f () =
  let start = Time_ns.now () in
  protect ~f ~finally:(fun () ->
    let finish = Time_ns.now () in
    print_s [%sexp (Time_ns.diff finish start : Time_ns.Span.t)])
;;

module Make (M : Arg) : S = struct
  let command =
    let main =
      let open Command.Let_syntax in
      let%map_open () = return ()
      and debug = flag "-debug" no_arg ~doc:" enable debug/progress printing"
      and time = flag "-time" no_arg ~doc:" measure and print runtime" in
      fun () ->
        (* re-exec self with [EULER_DEBUG] set *)
        if debug && not Debug_printing.Export.debug
        then
          never_returns
            (Unix.exec
               ()
               ~prog:Sys.executable_name
               ~argv:(Array.to_list Sys.argv)
               ~env:(`Extend [ "EULER_DEBUG", "1" ]));
        if time then time_unit M.main () else M.main ()
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
