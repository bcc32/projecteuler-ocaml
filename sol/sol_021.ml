open! Core
open! Import

module M = struct
  let problem = Number 21

  let sum_proper_divisors n =
    let sd = List.sum (module Int) (Number_theory.Int.divisors n) ~f:Fn.id in
    sd - n
  ;;

  let sum_proper_divisors =
    let cache = Int.Table.create () in
    fun n -> Hashtbl.findi_or_add cache n ~default:sum_proper_divisors
  ;;

  let amicable n =
    let sd = sum_proper_divisors n in
    sd <> n && sum_proper_divisors sd = n
  ;;

  let main () =
    Sequence.range 2 10_000
    |> Sequence.filter ~f:amicable
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;

  (* 31626
     20.397ms *)
end

include Solution.Make (M)
