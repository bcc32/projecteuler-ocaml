open! Core
open! Import

let example_x = 1487

let mem_binary haystack needle =
  Array.binary_search haystack ~compare:Int.compare `First_equal_to needle
  |> Option.is_some
;;

let main () =
  let is_prime = Number_theory.prime_sieve 10_000 in
  let visited = Array.create false ~len:10_000 in
  try
    for a = 1000 to 9999 do
      if not visited.(a) && is_prime.(a)
      then (
        let prime_permutations =
          a
          |> Number_theory.Int.As_base10.to_list
          |> Sequences.permutations ~compare:Int.compare
          |> Sequence.map ~f:(fun digits -> digits |> Number_theory.Int.As_base10.of_list)
          |> Sequence.filter ~f:(Array.get is_prime)
          |> Sequence.to_array
        in
        Array.sort prime_permutations ~compare:Int.compare;
        for i = 0 to Array.length prime_permutations - 1 do
          (* skip prime_permutations with leading zeros *)
          if prime_permutations.(i) >= 1000
          then (
            visited.(prime_permutations.(i)) <- true;
            for j = i + 1 to Array.length prime_permutations - 1 do
              let x = prime_permutations.(i) in
              let y = prime_permutations.(j) in
              let z = y + (y - x) in
              if mem_binary prime_permutations z && x <> example_x
              then (
                printf "%d%d%d\n" x y z;
                raise Exit)
            done)
        done)
    done
  with
  | Exit -> ()
;;

(* 1.749ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 296962999629 |}]
;;

include (val Solution.make ~problem:(Number 49) ~main)
