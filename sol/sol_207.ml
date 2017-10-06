open! Core

module M = struct
  let problem_number = 207

  (*
     4^t = 2^t + k
     4^t - 2^t = k
     n^2 - n = k where n = 2^t
     ==> the partition is perfect iff n is a power of 2.
  *)

  let main () =
    let ns = Euler.Int.natural_numbers ~init:2 () in
    Sequence.unfold_with ns ~init:(0, 0) ~f:(fun (part, perfect) n ->
      let new_state = part + 1, if Int.is_pow2 n then perfect + 1 else perfect in
      Yield ((new_state, n * n - n), new_state))
    |> Sequence.find_exn ~f:(fun ((part, perfect), _m) ->
      (* perfect / part < 1 / 12345 *)
      12345 * perfect < part)
    |> Tuple2.get2
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
