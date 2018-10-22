open! Core
open! Import

module M = struct
  let problem = Number 41

  let main () =
    with_return (fun { return } ->
      for n = 9 downto 0 do
        let digits = Array.init n ~f:(fun i -> n - i) in
        let rec loop () =
          let n =
            Array.to_sequence_mutable digits |> Number_theory.Int.int_of_digits
          in
          if Number_theory.Int.is_prime n
          then return n
          else if Sequences.next_permutation_inplace digits ~compare:Int.descending
          then loop ()
        in
        loop ()
      done;
      assert false)
    |> printf "%d\n"
  ;;

  (* 7652413
     229.525ms *)
end

include Solution.Make (M)
