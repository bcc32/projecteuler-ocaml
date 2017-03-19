open Core

module M = struct
  let problem_number = 67

  let path = "data/067.txt"

  let main () =
    Sol_018.read_triangle path
    |> Sol_018.max_sum_exn
    |> printf "%d\n"
end

include Solution.Make(M)
