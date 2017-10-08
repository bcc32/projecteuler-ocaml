open! Core

val run_length_encode : 'a list -> equal:'a Equal.t -> ('a * int) list

val is_palindrome : 'a list -> equal:'a Equal.t -> bool

val permutations : cmp:('a -> 'a -> int) -> 'a list -> 'a list Sequence.t
