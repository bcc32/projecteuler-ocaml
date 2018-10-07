open! Core
open! Import

module M = struct
  let problem = Number 56

  let main () =
    let hundred = Bigint.of_int 100 in
    let range = Number_theory.Bigint.range Bigint.one hundred in
    Sequence.cartesian_product range range
    |> Sequence.map ~f:(fun (a, b) -> Bigint.pow a b |> Number_theory.Bigint.sum_digits)
    |> Sequence.max_elt ~compare:Bigint.compare
    |> Option.value_exn
    |> printf !"%{Bigint}\n"
  ;;
end

include Solution.Make (M)
