open! Core
open! Import

module M = struct
  let problem = Number 45
  let prev = 40755

  let select_dups xs ys =
    Sequence.merge_with_duplicates xs ys ~cmp:Int.compare
    |> Sequence.filter_map ~f:(function
      | Left _ | Right _ -> None
      | Both (x, _) -> Some x)
  ;;

  let main () =
    let map_nats ~f = Number_theory.Int.natural_numbers () ~init:1 |> Sequence.map ~f in
    let triangle = map_nats ~f:(fun n -> n * (n + 1) / 2) in
    let pentagonal = map_nats ~f:(fun n -> n * ((3 * n) - 1) / 2) in
    let hexagonal = map_nats ~f:(fun n -> n * ((2 * n) - 1)) in
    select_dups (select_dups triangle pentagonal) hexagonal
    |> Sequence.drop_while ~f:(fun x -> x <= prev)
    |> Sequence.hd_exn
    |> printf "%d\n"
  ;;

  (* 1533776805
     8.8ms *)
end

include Solution.Make (M)
