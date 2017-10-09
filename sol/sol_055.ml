open! Core
open! Import
open Bignum.Std

module M = struct
  let problem = `Number 55

  let is_palindrome n =
    let s = Bigint.to_string n in
    s = String.rev s
  ;;

  let is_lychrel n =
    Sequence.unfold ~init:n ~f:(fun n ->
      let next = Bigint.(n + (n |> to_string |> String.rev |> of_string)) in
      Some (next, next))
    |> Fn.flip Sequence.take 50
    |> Sequence.exists ~f:is_palindrome
    |> not
  ;;

  let main () =
    Sequence.range 1 10_000
    |> Sequence.map ~f:Bigint.of_int
    |> Sequence.count ~f:is_lychrel
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
