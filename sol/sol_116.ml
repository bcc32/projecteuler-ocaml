open! Core
open! Import

module Cache_key = struct
  type t =
    { total_size : int
    ; block_size : int
    }
  [@@deriving compare, hash, sexp_of]
end

let rec ways_to_replace =
  let cache = Hashtbl.create (module Cache_key) in
  fun key ->
    Hashtbl.findi_or_add cache key ~default:(fun { total_size; block_size } ->
      if block_size > total_size
      then 1
      else (
        let gray_tile = ways_to_replace { total_size = total_size - 1; block_size } in
        let colored_tile =
          ways_to_replace { total_size = total_size - block_size; block_size }
        in
        gray_tile + colored_tile))
;;

let main () =
  (* Need to subtract 1 from each because we want to exclude empty
     "replacements". *)
  let red = ways_to_replace { total_size = 50; block_size = 2 } - 1 in
  let green = ways_to_replace { total_size = 50; block_size = 3 } - 1 in
  let blue = ways_to_replace { total_size = 50; block_size = 4 } - 1 in
  printf "%d\n" (red + green + blue)
;;

let%expect_test "answer" =
  main ();
  [%expect {| 20492570929 |}]
;;

include (val Solution.make ~problem:(Number 116) ~main)
