open! Core
open! Import

module Export = struct
  let debug =
    match Sys.getenv "EULER_DEBUG" with
    | None
    | Some "" -> false
    | Some _ -> true
  ;;
end
