open! Core
open! Import

let ( << ) f g = Fn.compose f g
let ( >> ) f g = Fn.compose g f
