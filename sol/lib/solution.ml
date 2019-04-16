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
  let summary =
    match M.problem with
    | Number n -> sprintf "Problem %d" n
    | Tagged { number; tag = _; description } ->
      sprintf "Problem %d (%s)" number description
    | Custom { name = _; description } -> description
  ;;

  let command_name =
    match M.problem with
    | Number n -> Int.to_string n
    | Tagged { number; tag; description = _ } -> sprintf "%d-%s" number tag
    | Custom { name; description = _ } -> name
  ;;

  let main debug time =
    (* FIXME: Don't set [debug] through an environment variable, make it a ref or a
       [Set_once]. *)
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
  ;;

  let debug_arg =
    Arg.(
      value
      & flag
      & info
          ~env:(env_var "EULER_DEBUG")
          [ "d"; "debug" ]
          ~doc:"Enable debug/progress printing")
  ;;

  let time_arg =
    Arg.(value & flag & info [ "t"; "time" ] ~doc:"Measure and print runtime")
  ;;

  let command = Term.(const main $ debug_arg $ time_arg, info command_name ~doc:summary)
end
