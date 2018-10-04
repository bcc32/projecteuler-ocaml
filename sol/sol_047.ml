open! Core
open! Import

module M = struct
  let problem = `Number 47

  let main () =
    let f n = List.length (Number_theory.Int.prime_factor n) = 4 in
    let numbers =
      Number_theory.Int.natural_numbers ~init:1 ()
      |> Sequence.unfold_with ~init:0 ~f:(fun s n ->
        let consec_ok = if f n then s + 1 else 0 in
        Yield ((n, consec_ok), consec_ok))
    in
    let last = Sequence.find_exn numbers ~f:(fun (_, c) -> c = 4) in
    let ans = fst last - 3 in
    printf "%d\n" ans
  ;;

  (* 134043
     261ms *)
end

include Solution.Make (M)
