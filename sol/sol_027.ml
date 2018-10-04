open! Core
open! Import

module M = struct
  let problem = `Number 27

  let count_primes a b =
    Number_theory.Int.natural_numbers ~init:0 ()
    |> Sequence.take_while ~f:(fun n -> Number_theory.Int.is_prime ((n * n) + (a * n) + b))
    |> Sequence.length
  ;;

  let main () =
    let range = Sequence.range ~stop:`inclusive (-999) 999 in
    Sequence.cartesian_product range range
    |> Sequence.map ~f:(fun (a, b) -> a * b, count_primes a b)
    |> Sequence.max_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
    |> Option.value_exn
    |> Tuple2.get1
    |> printf "%d\n"
  ;;

  (* -59231
     926ms *)
end

include Solution.Make (M)
