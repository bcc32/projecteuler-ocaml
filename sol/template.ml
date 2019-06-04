open! Core
open! Import

let problem = Number ___
let main () = raise_s [%message "unimplemented" [%here]]

include (val Solution.make ~problem ~main)
