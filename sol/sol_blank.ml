open! Core
open! Import

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

include (val Solution.make ~problem ~main)
