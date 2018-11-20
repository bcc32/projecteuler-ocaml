open! Core
open! Import

module M = struct
  let problem = Number ___
  let main () = raise_s [%message "unimplemented" [%here]]
end

include Solution.Make (M)
