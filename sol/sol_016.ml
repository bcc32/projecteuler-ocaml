open! Core
open! Import

module M = struct
  let problem = `Number 16

  let sum_digits n base =
    let open Bigint in
    let rec iter n acc = if n = zero then acc else iter (n / base) ((n % base) + acc) in
    iter n zero
  ;;

  let main () =
    let open Bigint in
    let base = of_int 2 in
    let expt = of_int 1000 in
    let num = pow base expt in
    sum_digits num (of_int 10) |> to_int_exn |> printf "%d\n"
  ;;
end

include Solution.Make (M)
