open! Core
open! Import

module M = struct
  let problem = Number 34

  (* Derived by observing that 9! * n < 10^(n - 1) for all n > 7. *)
  let max_digits = 7

  let main () =
    let factorial = Array.init 10 ~f:Number_theory.Int.factorial in
    Sequence.range 10 (Int.pow 10 max_digits)
    |> Sequence.filter ~f:(fun n ->
      n = Number_theory.Int.fold_digits n ~init:0 ~f:(fun ac d -> ac + factorial.(d)))
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
