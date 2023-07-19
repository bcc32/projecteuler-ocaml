open! Core
open! Import

(* TODO: Shadow Debug module so that functions automatically check
   [debug] flag. *)

module Export = struct
  let debug =
    match Sys.getenv "EULER_DEBUG" with
    | None | Some "" -> false
    | Some _ -> true
  ;;

  let debug_timing here name f =
    if not debug
    then f ()
    else (
      let start = Time_ns.now () in
      Debug.eprint_s [%message "starting" (name : string) (here : Source_code_position.t)];
      protect ~f ~finally:(fun () ->
        let end_ = Time_ns.now () in
        let elapsed = Time_ns.diff end_ start in
        Debug.eprint_s
          [%message
            "done"
              (name : string)
              (here : Source_code_position.t)
              (elapsed : Time_ns.Span.t)]))
  ;;
end
