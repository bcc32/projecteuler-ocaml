open! Core
open! Import

module M = struct
  let problem = Number 41

  let main () =
    Sequence.range ~stride:(-1) 9 0
    |> Sequence.find_map ~f:(fun n ->
      let digits = Array.init n ~f:(fun i -> n - i) in
      let rec loop () =
        let n =
          Array.to_sequence_mutable digits |> Number_theory.Int.int_of_digits
        in
        if Number_theory.Int.is_prime n
        then Some n
        else if Sequences.next_permutation_inplace digits ~compare:Int.descending
        then loop ()
        else None
      in
      loop ())
    |> Option.value_exn
    |> printf "%d\n"
  ;;

  (* 7652413
     229.525ms *)
end

include Solution.Make (M)
