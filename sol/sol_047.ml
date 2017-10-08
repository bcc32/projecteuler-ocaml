open! Core

module M = struct
  let problem = `Number 47

  let main () =
    let f n = n |> Euler.Int.prime_factor |> List.length |> Int.equal 4 in
    let numbers =
      Euler.Int.natural_numbers ~init:1 ()
      |> Sequence.unfold_with ~init:0 ~f:(fun s n ->
        let consec_ok = if f n then s + 1 else 0 in
        Yield ((n, consec_ok), consec_ok))
    in
    let last = Sequence.find_exn numbers ~f:(fun (_, c) -> c = 4) in
    let ans = fst last - 3 in
    printf "%d\n" ans
  ;;
end

include Euler.Solution.Make(M)
