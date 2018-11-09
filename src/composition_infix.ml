open! Core
open! Import

module Export = struct
  let ( << ) f g = Fn.compose f g
  let ( >> ) f g = Fn.compose g f
end

include Export
