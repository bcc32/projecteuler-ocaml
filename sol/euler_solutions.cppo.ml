open! Core
open! Import

let modules : (module Euler.Solution_intf.S) list = [
  #ext module_list
  #endext
]
;;
