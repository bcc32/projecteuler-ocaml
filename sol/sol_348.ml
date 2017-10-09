open! Core
open! Import

module M = struct
  let problem = `Number 348

  let palindromes_n_digits n =
    if n mod 2 = 0
    then (
      let lb = Int.pow 10 (n / 2  -  1) in
      let ub = Int.pow 10 (n / 2) in
      Sequence.range lb ub
      |> Sequence.map ~f:(fun n ->
        let d = Number_theory.Int.digits_of_int n in
        d @ List.rev d
        |> Sequence.of_list
        |> Number_theory.Int.int_of_digits))
    else (
      let lb = Int.pow 10 (n / 2)     in
      let ub = Int.pow 10 (n / 2 + 1) in
      Sequence.range lb ub
      |> Sequence.map ~f:(fun n ->
        let d = Number_theory.Int.digits_of_int n in
        d @ List.tl_exn (List.rev d)
        |> Sequence.of_list
        |> Number_theory.Int.int_of_digits))
  ;;

  let palindromes =
    Number_theory.Int.natural_numbers () ~init:1
    |> Sequence.bind ~f:palindromes_n_digits
  ;;

  (* greater than 1 *)
  let _squares =
    Number_theory.Int.natural_numbers () ~init:2
    |> Sequence.map ~f:(fun x -> x * x)
  ;;

  (* ditto *)
  let cubes =
    Number_theory.Int.natural_numbers () ~init:2
    |> Sequence.map ~f:(fun x -> x * x * x)
  ;;

  let main () =
    palindromes
    |> Sequence.filter ~f:(fun p ->
      let count =
        cubes
        |> Sequence.take_while ~f:(fun x -> x < p)
        |> Sequence.count ~f:(fun c ->
          Number_theory.Int.is_perfect_square (p - c))
      in
      count = 4)
    |> Fn.flip Sequence.take 5
    |> Sequence.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
