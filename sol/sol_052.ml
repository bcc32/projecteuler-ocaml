open! Core
open! Import

module M = struct
  let problem = Number 52

  let same_digits n =
    let sort_digits n =
      Number_theory.Int.digits_of_int n |> List.sort ~compare:Int.compare
    in
    let n_digits = sort_digits n in
    List.range 2 6 ~stop:`inclusive
    |> List.map ~f:(( * ) n)
    |> List.map ~f:sort_digits
    |> List.for_all ~f:(List.equal Int.equal n_digits)
  ;;

  let main () =
    Number_theory.Int.natural_numbers ~init:1 ()
    |> Sequence.find ~f:same_digits
    |> Option.value_exn
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
