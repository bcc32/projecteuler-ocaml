open! Core
open! Import

module M = struct
  let problem = `Number 36

  let main () =
    Sequence.range 1 1_000_000
    |> Sequence.filter ~f:(fun n ->
      let is_palindrome = Util.is_palindrome ~equal:Int.equal in
      is_palindrome (Number_theory.Int.digits_of_int n)
      && is_palindrome (Number_theory.Int.digits_of_int ~base:2 n))
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
