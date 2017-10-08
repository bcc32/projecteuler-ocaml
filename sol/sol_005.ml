open! Core

module M = struct
  let problem_number = 5

  let main () =
    List.fold ~init:1 ~f:Euler.Int.lcm (List.range ~stop:`inclusive 1 20)
    |> printf "%d\n"
  ;;
end

include Euler.Solution.Make(M)
