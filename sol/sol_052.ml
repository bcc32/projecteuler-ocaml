open! Core
open! Import

module M = struct
  let problem = Number 52
  let count_digits = Number_theory.Int.As_base10.fold ~init:0 ~f:(fun ac _ -> ac + 1)

  let same_digits n =
    let sort_digits n =
      Number_theory.Int.As_base10.to_list n |> List.sort ~compare:Int.compare
    in
    let n_digits = sort_digits n in
    count_digits (n * 6) = List.length n_digits
    && List.range 2 6 ~stop:`inclusive
       |> List.map ~f:(( * ) n)
       |> List.map ~f:sort_digits
       |> List.for_all ~f:([%equal: int list] n_digits)
  ;;

  let main () =
    Number_theory.Int.natural_numbers ~init:1 ()
    |> Sequence.find ~f:same_digits
    |> Option.value_exn
    |> printf "%d\n"
  ;;

  (* 142857
     138.157ms *)
end

include Solution.Make (M)
