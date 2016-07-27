open Core.Std
open Core_extended.Std

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
    match Sys.getenv "USE_BITARRAY" with
    | Some _ ->
      let can_sum = Bitarray.create (limit + 1) in
      let rec iter outer inner =
        match outer, inner with
        | [], []        -> ()
        | [], _         -> assert false
        | (_ :: tl), [] -> iter tl tl
        | (x :: xs), (y :: ys) ->
          if x + y > limit
          then iter xs xs
          else (
            Bitarray.set can_sum (x + y) true;
            iter outer ys
          )
      in
      let abundant_numbers = force abundant_numbers in
      iter abundant_numbers abundant_numbers;
      let sum = ref 0 in
      for i = 0 to limit do
        if not (Bitarray.get can_sum i)
        then sum := !sum + i
      done;
      printf "%d\n" !sum
    | None ->
      let can_sum = Array.create false ~len:(limit + 1) in
      let rec iter outer inner =
        match outer, inner with
        | [], []        -> ()
        | [], _         -> assert false
        | (_ :: tl), [] -> iter tl tl
        | (x :: xs), (y :: ys) ->
          if x + y > limit
          then iter xs xs
          else (
            can_sum.(x + y) <- true;
            iter outer ys
          )
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
