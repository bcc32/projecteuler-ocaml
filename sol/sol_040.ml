open! Core
open! Import

let champernowne's_constant =
  Sequence.memoize
    (let open Sequence.Let_syntax in
     let%bind nat = Number_theory.Int.natural_numbers () ~init:1 in
     Number_theory.Int.digits_of_int nat |> Sequence.of_list)
;;

module M = struct
  let problem = Number 40
  let indices = [ 1; 10; 100; 1_000; 10_000; 100_000; 1_000_000 ]

  let main () =
    List.fold indices ~init:1 ~f:(fun ac i ->
      ac * Sequence.nth_exn champernowne's_constant (i - 1))
    |> printf "%d\n"
  ;;

  (* 210
     262.095ms *)
end

include Solution.Make (M)
