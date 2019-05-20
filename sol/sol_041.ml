open! Core
open! Import

module M = struct
  let problem = Number 41

  let main () =
    with_return (fun { return } ->
      for n = 9 downto 0 do
        let digits = List.init n ~f:(fun i -> n - i) in
        Sequences.iter_permutations digits ~compare:Int.descending ~f:(fun digits ->
          let n =
            Array.Permissioned.to_sequence_mutable digits
            |> Number_theory.Int.As_base10.of_sequence
          in
          if Number_theory.Int.is_prime n then return n)
      done;
      assert false)
    |> printf "%d\n"
  ;;

  (* 349.457ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 7652413 |}]
  ;;
end

include Solution.Make (M)
