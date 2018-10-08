(** Functions for manipulating sequences of elements. *)

open! Core
open! Import

(** [digits_of_string] parses a numeric string into a list of digits,
    left-to-right.  Handles bases up to 10. *)
val digits_of_string : string -> int list

val is_palindrome : 'a list -> equal:'a Equal.t -> bool

(** {1 Run-length encoding}

    See {{:https://en.wikipedia.org/wiki/Run-length_encoding}Wikipedia} for
    background. *)

(** [run_length_encode xs] returns a list of [(element, count)] pairs in the
    same order the elements appear in the list.  Every pair of consecutive
    elements in the returned list are different. *)
val run_length_encode : 'a list -> equal:'a Equal.t -> ('a * int) list

(** [run_length_decode] is the inverse of [run_length_encode]. *)
val run_length_decode : ('a * int) list -> 'a list

(** {1 Permutations}

    These functions work correctly in the presence of duplicate elements. *)

(** [next_permutation_inplace a] attempts to permute array [a] to the next
    permutation according to the total ordering defined by [compare].  It returns
    true if successful, or false if there are no more permutations (the array is
    in non-increasing order).

    For example, the next permutation after [[| 2; 5; 4 |]] is [[| 4; 2; 5 |]]
    (using the natural order on ints). *)
val next_permutation_inplace : 'a array -> compare:('a -> 'a -> int) -> bool

(** [prev_permutation_inplace a] is equivalent to [next_permutation_inplace]
    with a reversed total ordering. *)
val prev_permutation_inplace : 'a array -> compare:('a -> 'a -> int) -> bool

(** [permutations xs] returns a sequence of all of the permutations of [xs] in
    lexicographic order, starting with sorted ascending and ending with sorted
    descending. *)
val permutations : 'a list -> compare:('a -> 'a -> int) -> 'a list Sequence.t
