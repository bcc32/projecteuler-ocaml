open! Core
open! Import

(* Consider a sequence of coin moves $(c_0, c_1, c_2, ..., c_{n-1})$.  Each
   index in the sequence represents the first coin turned.  Each coin may be
   either turned or not turned; coin turns may be arbitrarily reordered.
   Therefore, a state can be fully represented by a subset of the indices ${ 0,
   1, ..., n - 1 }$, where the elements of that set represent the coins turned.

   A solvable state can, after applying a sequence of moves, reach the all-heads
   state.  Since moves are their own inverses, and sequences of moves are also
   their own inverses, we may say that a solvable state is also a state which
   can be reached /from/ the all-heads state.

   We should count the number of solvable states by the number of sequences of
   moves.  Some sequences of moves may be equivalent to other sequences of
   moves; in order to avoid overcounting, we should eliminate any sequences of
   moves which are equivalent to each other.

   There are 2^n possible sequences of moves.

   Consider two sequences of moves, S and T.  If S and T are different but
   /equivalent/, i.e., they represent a different set of coins turned but the
   same eventual state, we may say that S + T = 0.

   Because of symmetry (hand-waving), the number of equivalent move sequences of
   any given sequence S (including S itself) is the same as the number of move
   sequences equivalent to the null move sequence.

   Therefore, the number of distinct states reachable from the all-heads state
   is equal to 2^n divided by the number of move sequences equivalent to the
   null move sequence, including the null move sequence. *)

module For_63_bits = struct
  let get_bit_norm ~n_bits int ~bit =
    let bit = if bit < 0 then bit + n_bits else bit in
    (int lsr bit) land 1
  ;;

  let move_sequences_equivalent_to_null n k =
    Sequence.range 0 (Int.pow 2 n)
    |> Sequence.count ~f:(fun move_bits ->
      let state =
        let state_bits =
          List.init n ~f:(fun state_bit ->
            List.init k ~f:(fun move_bit ->
              get_bit_norm ~n_bits:n move_bits ~bit:(state_bit - move_bit))
            |> List.fold ~init:0 ~f:( lxor ))
        in
        List.fold state_bits ~init:0 ~f:(fun acc bit -> (acc lsl 1) lor bit)
      in
      state = 0)
  ;;

  let distinct_states n k = Int.pow 2 n / move_sequences_equivalent_to_null n k

  let%expect_test "F(3,2)" =
    distinct_states 3 2 |> [%sexp_of: int] |> print_s;
    [%expect {| 4 |}]
  ;;

  let%expect_test "F(8,3)" =
    distinct_states 8 3 |> [%sexp_of: int] |> print_s;
    [%expect {| 256 |}]
  ;;

  let%expect_test "F(9,3)" =
    distinct_states 9 3 |> [%sexp_of: int] |> print_s;
    [%expect {| 128 |}]
  ;;
end

let main () = raise_s [%message "unimplemented" [%here]]

include (val Solution.make ~problem:(Number 728) ~main)
