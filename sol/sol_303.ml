open! Core
open! Import

module M = struct
  let problem = `Number 303

  let small_digits =
    Number_theory.Int.natural_numbers () ~init:1
    |> Sequence.map ~f:(fun n ->
      let rec reverse_digits n ac =
        match n with
        | 0 -> ac
        | n -> reverse_digits (n / 10) (10 * ac + n mod 10)
      in
      let rec loop n ac =
        match n with
        | 0 -> reverse_digits ac 0
        | n -> loop (n / 3) (10 * ac + n mod 3)
      in
      loop n 1 / 10)
  ;;

  let f n = Sequence.find_exn small_digits ~f:(fun x -> x mod n = 0)

  let fn_n n = f n / n

  let main () =
    Sequence.range 1 1000 ~stop:`inclusive
    |> Sequence.sum (module Int) ~f:fn_n
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
