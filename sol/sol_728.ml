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

let main () = raise_s [%message "unimplemented" [%here]]

include (val Solution.make ~problem:(Number ___) ~main)
