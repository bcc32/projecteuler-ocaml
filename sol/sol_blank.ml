open! Core
open! Import

let main () =
  if debug then Debug.eprint "<debugging output>";
  printf "<dummy answer>\n"
;;

include (val Solution.make
               ~problem:
                 (Custom
                    { name = "blank"
                    ; description =
                        "dummy solution for benchmarking solution apparatus overhead"
                    })
               ~main)
