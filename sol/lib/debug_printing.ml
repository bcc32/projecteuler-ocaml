open! Core
open! Import

(* TODO: Shadow Debug module so that functions automatically check
   [debug] flag.  *)

module Export = struct
  let debug =
    match Sys.getenv "EULER_DEBUG" with
    | None
    | Some "" -> false
    | Some _ -> true
  ;;

  (* FIXME: Use [@sexp.option] attribute when public ppx_sexp_conv supports it. *)
  let debug_timing ?here ?task f x =
    if not debug
    then f x
    else (
      let start = Time_ns.now () in
      Debug.eprint_s
        [%message
          "starting"
            (task : string sexp_option)
            (here : Source_code_position.t sexp_option)];
      protectx ~f x ~finally:(fun _ ->
        let end_ = Time_ns.now () in
        let elapsed = Time_ns.diff end_ start in
        Debug.eprint_s
          [%message
            "done"
              (task : string sexp_option)
              (here : Source_code_position.t sexp_option)
              (elapsed : Time_ns.Span.t)]))
  ;;
end
