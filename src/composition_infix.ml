open! Core

module Export = struct
  let ( << ) f g = Fn.compose f g
  let ( >> ) f g = Fn.compose g f
end

include Export
