open! Core

module M = struct
  let problem_number = 46

  let cannot_be_written n =
    let upper_bound = Float.(of_int n / 2.0 |> sqrt |> to_int) in
    Sequence.range ~stop:`inclusive 1 upper_bound
    |> Sequence.exists ~f:(fun s ->
      n - 2 * s * s
      |> Euler.Int.is_prime)
    |> not
  ;;

  let main () =
    Sequence.unfold ~init:3 ~f:(fun s -> Some (s, s + 2))
    |> Sequence.filter ~f:(Fn.non Euler.Int.is_prime)
    |> Sequence.find ~f:cannot_be_written
    |> Option.value_exn
    |> printf "%d\n"
  ;;
end

include Euler.Solution.Make(M)
