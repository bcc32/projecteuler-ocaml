open! Core
open! Import

module M = struct
  let problem = Number 67

  let main () =
    Problem_067.data |> Sol_018.parse_triangle |> Sol_018.max_sum_exn |> printf "%d\n"
  ;;

  (* 7273
     0.903ms *)
end

include Solution.Make (M)
