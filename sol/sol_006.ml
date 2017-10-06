open! Core

module M = struct
  let problem_number = 6

  let hund =
    List.range ~stop:`inclusive 1 100

  let sum =
    List.sum (module Int) ~f:Fn.id

  let sqr x =
    x * x

  let main () =
    sqr (sum hund) - sum (List.map ~f:sqr hund)
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
