open! Core
open! Import

val digits_of_string : string -> int list
val run_length_encode : 'a Equal.t -> 'a list -> ('a * int) list
val run_length_decode : ('a * int) list -> 'a list
val is_palindrome : 'a Equal.t -> 'a list -> bool

(** [next_permutation_inplace a] attempts to permute array [a] to the next
    permutation according to the total ordering defined by [compare]. It returns
    true if successful, or false if there are no more permutations (the array is
    in non-increasing order).

    For example, the next permutation after [[| 2; 5; 4 |]] is [[| 4; 2; 5 |]]
    (using the natural order on ints). *)
val next_permutation_inplace : 'a array -> compare:('a -> 'a -> int) -> bool

(** [prev_permutation_inplace a] is equivalent to [next_permutation_inplace]
    with a reversed total ordering. *)
val prev_permutation_inplace : 'a array -> compare:('a -> 'a -> int) -> bool

val permutations : 'a list -> compare:('a -> 'a -> int) -> 'a list Sequence.t
