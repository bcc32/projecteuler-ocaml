open! Core
open! Import

module M = struct
  let problem = `Number 34

  (* Derived by observing that 9! * n < 10^(n - 1) for all n > 7. *)
  let max_digits = 7

  let main () =
    Sequence.range 10 (Int.pow 10 max_digits)
    |> Sequence.filter ~f:(fun n ->
      Number_theory.Int.digits_of_int n
      |> List.sum (module Int) ~f:Number_theory.Int.factorial
      |> Int.equal n)
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
