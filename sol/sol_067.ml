open! Core
open! Import

module M = struct
  let problem = Number 67
  let path = "data/067.txt"
  let main () = path |> Sol_018.read_triangle |> Sol_018.max_sum_exn |> printf "%d\n"
end

include Solution.Make (M)
