open! Core
open! Import

module M = struct
  let problem = Number 2

  let fibs n =
    let rec iter a b acc = if b > n then acc else iter b (a + b) (b :: acc) in
    iter 0 1 [] |> List.rev
  ;;

  let main () =
    fibs 4000000
    |> List.filter ~f:(fun x -> x mod 2 = 0)
    |> List.fold ~init:0 ~f:( + )
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
