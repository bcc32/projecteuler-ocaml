open Core

module M = struct
  let problem_number = 36

  let main () =
    Sequence.range 1 1_000_000
    |> Sequence.filter ~f:(fun n ->
      let is_palindrome = Euler.is_palindrome ~equal:Int.equal in
      is_palindrome (Euler.Int.digits_of_int n)
      && is_palindrome (Euler.Int.digits_of_int ~base:2 n))
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
