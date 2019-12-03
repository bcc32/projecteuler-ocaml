open! Core
include Cmdliner
include Euler_solution_helpers

let error_s sexp =
  Or_error.error_s sexp |> Result.map_error ~f:(fun e -> `Msg (Error.to_string_hum e))
;;
