open Core

module M = struct
  let problem_number = 19

  let main () =
    let start = Date.of_string "1901-01-01" in
    let stop  = Date.of_string "2000-12-31" in
    Sequence.unfold ~init:start ~f:(fun d ->
      Some (d, Date.add_months d 1)
    )
    |> Sequence.take_while ~f:(Date.(>=) stop)
    |> Sequence.count ~f:(fun d ->
      Date.day_of_week d = Day_of_week.Sun
    )
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
