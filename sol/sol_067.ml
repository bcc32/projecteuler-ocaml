open! Core
open! Import

module M = struct
  let problem = Number 67

  let main () =
    Problem_067.data
    |> Parse.space_separated_grid ~conv:Int.of_string
    |> Sol_018.max_sum_exn
    |> printf "%d\n"
  ;;

  (* 7273
     0.903ms *)
end

include Solution.Make (M)
