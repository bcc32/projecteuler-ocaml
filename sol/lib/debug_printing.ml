open! Core
open! Import

(* TODO: Shadow Debug module so that functions automatically check
   [debug] flag.  *)

module Export = struct
  let debug =
    match Sys.getenv "EULER_DEBUG" with
    | None | Some "" -> false
    | Some _ -> true
  ;;

  (* TODO: Make [here] and [task] required. *)
  let debug_timing ?here ?task f x =
    if not debug
    then f x
    else (
      let start = Time_ns.now () in
      Debug.eprint_s
        [%message
          "starting"
            (task : (string option[@sexp.option]))
            (here : (Source_code_position.t option[@sexp.option]))];
      protectx ~f x ~finally:(fun _ ->
        let end_ = Time_ns.now () in
        let elapsed = Time_ns.diff end_ start in
        Debug.eprint_s
          [%message
            "done"
              (task : (string option[@sexp.option]))
              (here : (Source_code_position.t option[@sexp.option]))
              (elapsed : Time_ns.Span.t)]))
  ;;
end
