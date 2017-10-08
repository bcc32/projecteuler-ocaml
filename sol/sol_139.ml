open! Core

module M = struct
  let problem_number = 139

  let pythag_triples : (int * int * int) Sequence.t =
    Euler.Int.natural_numbers ~init:5 ()
    |> Sequence.concat_map ~f:(fun c ->
      Sequence.range 4 c
      |> Sequence.map ~f:(fun b -> (b, c)))
    |> Sequence.filter_map ~f:(fun (b, c) ->
      let a = c * c - b * b |> float |> sqrt |> Float.iround_nearest_exn in
      if a < b && a * a + b * b = c * c
      then Some (a, b, c)
      else None)
  ;;

  let main () =
    (*
    pythag_triples
    |> Sequence.take_while ~f:(fun (a, b, c) -> a + b + c < 100_000_000)
    |> Sequence.count ~f:(fun (a, b, c) -> c % (b - a) = 0)
    |> printf "%d\n"
    *)
    pythag_triples
    |> Sequence.iter ~f:(printf !"%{sexp: int * int * int}\n%!")
  ;;
end

include Euler.Solution.Make(M)
