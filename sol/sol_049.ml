open! Core
open! Import

module M = struct
  let problem = Number 49

  let main () =
    let is_prime = Number_theory.prime_sieve 10_000 in
    let checked = Array.create false ~len:10_000 in
    try
      for a = 1000 to 9999 do
        if not checked.(a)
        then
          if is_prime.(a)
          then (
            let numbers =
              a
              |> Number_theory.Int.digits_of_int
              |> Sequences.permutations ~compare:Int.compare
              |> Sequence.map ~f:(fun digits ->
                digits |> Sequence.of_list |> Number_theory.Int.int_of_digits)
              |> Sequence.filter ~f:(Array.get is_prime)
              |> Sequence.to_array
            in
            Array.sort numbers ~compare:Int.compare;
            for i = 0 to Array.length numbers - 1 do
              checked.(numbers.(i)) <- true;
              for j = i + 1 to Array.length numbers - 1 do
                let x = numbers.(i) in
                let y = numbers.(j) in
                if Array.binary_search
                     numbers
                     ~compare:Int.compare
                     `First_equal_to
                     (y + (y - x))
                   |> Option.is_some
                && x >= 1000
                && x <> 1487
                then (
                  printf "%d%d%d\n" x y (y + (y - x));
                  raise Exit)
              done
            done)
      done
    with
    | Exit -> ()
  ;;

  (* 296962999629
     1.749ms *)
end

include Solution.Make (M)
