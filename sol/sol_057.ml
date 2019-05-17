open! Core
open! Import

module M = struct
  let problem = Number 57

  let is_more_num_digits r =
    let num = r |> Bignum.num_as_bigint |> Bigint.to_string |> String.length in
    let den = r |> Bignum.den_as_bigint |> Bigint.to_string |> String.length in
    num > den
  ;;

  let main () =
    let expansions =
      let open Bignum.O in
      let init = of_int 1 + (1 // 2) in
      Sequence.unfold_step ~init ~f:(fun s ->
        let next = of_int 1 + (of_int 1 / (of_int 1 + s)) in
        Yield (s, next))
    in
    expansions
    |> Fn.flip Sequence.take 1000
    |> Sequence.count ~f:is_more_num_digits
    |> printf "%d\n"
  ;;

  (* 153
     19.716ms *)
end

include Solution.Make (M)
