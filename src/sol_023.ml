open Core.Std

module M = struct
  let problem_number = 23

  let limit = 28123

  let is_abundant n =
    let divisor_sum =
      Euler.divisors n
      |> List.fold ~init:0 ~f:(+)
    in
    divisor_sum > 2 * n

  let abundant_numbers =
    lazy (
      Euler.natural_numbers ~init:12 ()
      |> Sequence.filter ~f:is_abundant
      |> Sequence.take_while ~f:((>=) limit)
      |> Sequence.to_list
    )

  let main () =
    let can_sum = Array.create false ~len:(limit + 1) in
    let rec iter outer inner =
      match outer, inner with
      | [], [] -> ()
      | [], _  ->
        failwiths "unexpected state" (outer, inner)
          [%sexp_of: int list * int list]
      | (_ :: tl), [] -> iter tl tl
      | (x :: xs), (y :: ys) ->
        if x + y > limit
        then iter xs xs
        else (
          can_sum.(x + y) <- true;
          iter outer ys)
    in
    let abundant_numbers = force abundant_numbers in
    iter abundant_numbers abundant_numbers;
    let sum = ref 0 in
    for i = 0 to limit do
      if not can_sum.(i)
      then sum := !sum + i
    done;
    printf "%d\n" !sum
end

include Solution.Make(M)
