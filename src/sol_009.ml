open Core.Std

module M = struct
  let problem_number = 9

  let main () =
    with_return_option (fun r ->
      for c = 5 to 500 do
        for b = 4 to c - 1 do
          let a = 1000 - b - c in
          if Euler.is_pythagorean_triple a b c
          then r.return (a * b * c)
        done
      done
    )
    |> Option.value_exn
    |> printf "%d\n"
end

include Solution.Make(M)
