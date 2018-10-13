open! Core
open! Import

module M = struct
  let problem = Number 45
  let prev = 40755

  let select_dups (s : ('a, 'a) Sequence.Merge_with_duplicates_element.t Sequence.t) =
    Sequence.filter_map s ~f:(function
      | Left _ | Right _ -> None
      | Both (x, _) -> Some x)
  ;;

  let main () =
    let map_nats ~f = Number_theory.Int.natural_numbers () ~init:1 |> Sequence.map ~f in
    let triangle =
      map_nats ~f:(fun n ->
        if debug then Debug.eprintf "tri %d" n;
        n * (n + 1) / 2)
    in
    let pentagonal =
      map_nats ~f:(fun n ->
        if debug then Debug.eprintf "pent %d" n;
        n * ((3 * n) - 1) / 2)
    in
    let hexagonal =
      map_nats ~f:(fun n ->
        if debug then Debug.eprintf "hex %d" n;
        n * ((2 * n) - 1))
    in
    Sequence.merge_with_duplicates triangle pentagonal ~cmp:Int.compare
    |> select_dups
    |> Sequence.merge_with_duplicates hexagonal ~cmp:Int.compare
    |> select_dups
    |> Sequence.drop_while ~f:(fun x -> x <= prev)
    |> Sequence.hd_exn
    |> printf "%d\n"
  ;;

  (* 1533776805
     8.8ms *)
end

include Solution.Make (M)
