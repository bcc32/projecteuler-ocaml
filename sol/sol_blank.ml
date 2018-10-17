open! Core
open! Import

module M = struct
  let problem =
    Custom
      { name = "blank"
      ; description = "dummy solution for benchmarking solution apparatus overhead"
      }
  ;;

  let main () =
    if debug then Debug.eprint "<debugging output>";
    printf "<dummy answer>\n"
  ;;
end

include Solution.Make (M)
