open! Core
open! Import

module M = struct
  let problem = Number 47

  let main () =
    let distinct_prime_factors =
      Number_theory.Int.natural_numbers ~init:2 ()
      |> Sequence.map ~f:(fun n ->
        if debug then Debug.eprintf "factoring %d" n;
        n, Number_theory.Int.prime_factor n |> List.length)
    in
    let each_consecutive_4 =
      let a = distinct_prime_factors |> Sequence.memoize in
      let b = a |> Sequence.tl_eagerly_exn in
      let c = b |> Sequence.tl_eagerly_exn in
      let d = c |> Sequence.tl_eagerly_exn in
      (* TODO Replace this with a zip-based applicative. *)
      Sequence.zip (Sequence.zip a b) (Sequence.zip c d)
      |> Sequence.map ~f:(fun ((a, b), (c, d)) -> a, b, c, d)
    in
    each_consecutive_4
    |> Sequence.find_map ~f:(function (a, apf), (_, bpf), (_, cpf), (_, dpf) ->
      (* Ideally, we'd like to skip multiple 4-tuples immediately if, e.g.,
         [apf <> 4]. *)
      if apf = 4 && bpf = 4 && cpf = 4 && dpf = 4 then Some a else None)
    |> uw
    |> printf "%d\n"
  ;;

  (* 134043
     210.016ms *)
end

include Solution.Make (M)
