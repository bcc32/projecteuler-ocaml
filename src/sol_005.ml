open Core.Std

module M = struct
  let problem_number = 5

  let rec gcd a b =
    if b = 0
    then a
    else gcd b (a mod b)

  let lcm a b =
    a / (gcd a b) * b

  let main () =
    List.fold ~init:1 ~f:lcm (List.range ~stop:`inclusive 1 20)
    |> printf "%d\n"
end

include Solution.Make(M)
