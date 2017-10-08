open! Core

module M = struct
  let problem = `Number 39

  let main () =
    Sequence.range ~stop:`inclusive 12 1000
    |> Sequence.map ~f:(fun p ->
      let solutions =
        Sequence.range 5 (p / 2)
        |> Sequence.sum (module Int) ~f:(fun c ->
          Sequence.range ((p - c) / 2) c
          |> Sequence.count ~f:(fun b ->
            let a = p - c - b in
            Euler.is_pythagorean_triple a b c))
      in
      p, solutions)
    |> Sequence.max_elt ~cmp:(fun (_, a) (_, b) -> Int.compare a b)
    |> uw
    |> fst
    |> printf "%d\n"
  ;;
end

include Euler.Solution.Make(M)
