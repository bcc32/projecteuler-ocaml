open! Core
open! Import

let main () = raise_s [%message "unimplemented" [%here]]

include (val Solution.make ~problem:(Number ___) ~main)
