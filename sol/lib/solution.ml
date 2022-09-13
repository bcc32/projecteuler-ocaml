open! Core
open! Import
include Solution_intf

let time_unit f () =
  let start = Time_ns.now () in
  protect ~f ~finally:(fun () ->
    let finish = Time_ns.now () in
    print_s [%sexp (Time_ns.diff finish start : Time_ns.Span.t)])
;;

type t = ?print_debug:bool -> ?print_elapsed_time:bool -> unit -> unit

let make ~(problem : Solution_id.t) ~main =
  (module struct
    let name =
      match problem with
      | Number n -> Int.to_string n
      | Tagged { number; tag; description = _ } -> sprintf "%d-%s" number tag
      | Custom { name; description = _ } -> name
    ;;

    let description =
      match problem with
      | Number n -> sprintf "Problem %d" n
      | Tagged { number; tag = _; description } ->
        sprintf "Problem %d (%s)" number description
      | Custom { name = _; description } -> description
    ;;

    let run ?(print_debug = false) ?(print_elapsed_time = false) () =
      (* FIXME: Don't set [debug] through an environment variable, make it a ref or a
         [Set_once]. *)
      (* re-exec self with [EULER_DEBUG] set *)
      if print_debug && not Debug_printing.Export.debug
      then
        never_returns
          (Core_unix.exec
             ()
             ~prog:Sys_unix.executable_name
             ~argv:(Array.to_list (Sys.get_argv ()))
             ~env:(`Extend [ "EULER_DEBUG", "1" ]));
      if print_elapsed_time then time_unit main () else main ()
    ;;
  end : S)
;;
