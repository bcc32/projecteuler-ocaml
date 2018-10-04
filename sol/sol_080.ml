open! Core
open! Import

module M = struct
  let problem = `Number 80

  let sqrt n =
    let open Bignum in
    Numerics.Bignum.newton's_method
      ~f:(fun x -> (x * x) - n)
      ~f':(fun x -> of_int 2 * x)
      ~init:one
      ~epsilon:Bignum.(tenth ** 101)
  ;;

  let hundred_decimal_digits n =
    Bignum.(n * (ten ** 101) |> round_as_bigint |> Option.value_exn)
  ;;

  let digital_sum n =
    n
    |> Bignum.of_int
    |> sqrt
    |> hundred_decimal_digits
    |> Bigint.to_string
    |> String.subo ~len:100
    |> Util.digits_of_string
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let main () =
    let range = List.range 1 100 ~stop:`inclusive |> Int.Set.of_list in
    let squares =
      List.range 1 10 ~stop:`inclusive
      |> List.map ~f:(Fn.flip Int.pow 2)
      |> Int.Set.of_list
    in
    let irrational_sqrts = Set.diff range squares in
    irrational_sqrts
    |> Set.to_sequence
    |> Sequence.sum (module Int) ~f:digital_sum
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
