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

  let%expect_test "table" =
    let max = 15 in
    printf "%2d\t" 0;
    for k = 1 to max do
      printf "%d\t" k
    done;
    printf "\n";
    for n = 1 to max do
      printf "%2d\t" n;
      for k = 1 to n do
        printf "%d\t" (distinct_states n k)
      done;
      printf "\n"
    done;
    [%expect
      {|
       0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15
       1	2
       2	4	2
       3	8	4	2
       4	16	8	16	2
       5	32	16	32	16	2
       6	64	32	16	16	64	2
       7	128	64	128	64	128	64	2
       8	256	128	256	32	256	128	256	2
       9	512	256	128	256	512	64	512	256	2
      10	1024	512	1024	256	64	512	1024	256	1024	2
      11	2048	1024	2048	1024	2048	1024	2048	1024	2048	1024	2
      12	4096	2048	1024	512	4096	128	4096	256	1024	2048	4096	2
      13	8192	4096	8192	4096	8192	4096	8192	4096	8192	4096	8192	4096	2
      14	16384	8192	16384	4096	16384	8192	256	4096	16384	8192	16384	4096	16384	2
      15	32768	16384	8192	16384	2048	4096	32768	16384	8192	1024	32768	4096	32768	16384	2 |}]
  ;;

  (* For each row n, the entries mostly alternate between two different powers
     of 2, 2^n and 2^(n - 1), except:

     For n = k, F(n, k) = 2.

     For gcd(n, k) > 1, the entries may be a smaller power of 2.

     In particular,

     F(n, k) = 2 if n = k

     F(n, k) = 2^(n - a) where

     a = k mod 2 + b

     b = 2p if n and k share a prime factor greater than 2 (p is the index of
     that prime number, where p = 1 if the prime factor is 3)

     F(n, k) = 2^n otherwise *)

  let%expect_test "pattern" =
    let max = 15 in
    for n = 1 to max do
      for k = 1 to n do
        let base = Int.pow 2 n in
        let diff = Int.floor_log2 (base / distinct_states n k) in
        if diff > 0 then printf "%d %d %d\n" n k diff
      done
    done;
    [%expect
      {|
      2 2 1
      3 2 1
      3 3 2
      4 2 1
      4 4 3
      5 2 1
      5 4 1
      5 5 4
      6 2 1
      6 3 2
      6 4 2
      6 6 5
      7 2 1
      7 4 1
      7 6 1
      7 7 6
      8 2 1
      8 4 3
      8 6 1
      8 8 7
      9 2 1
      9 3 2
      9 4 1
      9 6 3
      9 8 1
      9 9 8
      10 2 1
      10 4 2
      10 5 4
      10 6 1
      10 8 2
      10 10 9
      11 2 1
      11 4 1
      11 6 1
      11 8 1
      11 10 1
      11 11 10
      12 2 1
      12 3 2
      12 4 3
      12 6 5
      12 8 4
      12 9 2
      12 10 1
      12 12 11
      13 2 1
      13 4 1
      13 6 1
      13 8 1
      13 10 1
      13 12 1
      13 13 12
      14 2 1
      14 4 2
      14 6 1
      14 7 6
      14 8 2
      14 10 1
      14 12 2
      14 14 13
      15 2 1
      15 3 2
      15 4 1
      15 5 4
      15 6 3
      15 8 1
      15 9 2
      15 10 5
      15 12 3
      15 14 1
      15 15 14 |}]
  ;;

  let s n =
    Sequence.range 1 n ~stop:`inclusive
    |> Sequence.sum
         (module Int)
         ~f:(fun n ->
           Sequence.range 1 n ~stop:`inclusive
           |> Sequence.sum (module Int) ~f:(fun k -> distinct_states n k))
  ;;

  let%expect_test "S(3)" =
    print_s [%sexp (s 3 : int)];
    [%expect {| 22 |}]
  ;;

  let%expect_test "S(10)" =
    print_s [%sexp (s 10 : int)];
    [%expect {| 10444 |}]
  ;;
end

let main () = raise_s [%message "unimplemented" [%here]]

include (val Solution.make ~problem:(Number 728) ~main)
