open! Core
open! Import

module M = struct
  let problem = Number 46

  let cannot_be_written n =
    let upper_bound = Number_theory.Int.isqrt (n / 2) in
    not
      (Sequence.range ~stop:`inclusive 1 upper_bound
       |> Sequence.exists ~f:(fun s -> n - (2 * s * s) |> Number_theory.Int.is_prime))
  ;;

  let main () =
    Sequence.unfold ~init:3 ~f:(fun s -> Some (s, s + 2))
    |> Sequence.filter ~f:(Fn.non Number_theory.Int.is_prime)
    |> Sequence.find ~f:cannot_be_written
    |> Option.value_exn
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
