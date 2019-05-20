open! Core
open! Import

module M = struct
  let problem = Number 56

  let main () =
    let range = Number_theory.Bigint.range Bigint.one (Bigint.of_int 100) in
    Sequence.cartesian_product range range
    |> Sequence.map ~f:(fun (a, b) ->
      Bigint.pow a b |> Number_theory.Bigint.As_base10.sum (module Bigint) ~f:Fn.id)
    |> Sequence.max_elt ~compare:Bigint.compare
    |> Option.value_exn
    |> printf !"%{Bigint}\n"
  ;;

  (* 972
     134.391ms *)
end

include Solution.Make (M)
