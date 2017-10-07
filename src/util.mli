open! Core

val run_length_encode : 'a list -> ('a * int) list

val is_palindrome : 'a list -> equal:('a -> 'a -> bool) -> bool

val permutations : cmp:('a -> 'a -> int) -> 'a list -> 'a list Sequence.t
