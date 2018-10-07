open! Core
open! Import

module M = struct
  let problem = Number 48

  let modulus =
    let ten = Bigint.of_int 10 in
    Bigint.pow ten ten
  ;;

  let main () =
    let sum =
      Sequence.range ~stop:`inclusive 1 1000
      |> Sequence.sum
           (module Bigint)
           ~f:(fun n ->
             let n = Bigint.of_int n in
             Bigint.pow n n)
    in
    Bigint.(sum % modulus) |> printf !"%{Bigint}\n"
  ;;
end

include Solution.Make (M)
