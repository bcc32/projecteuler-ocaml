open! Core
open! Import

let grid () = Problem_345.data |> Parse.space_separated_grid ~conv:Int.of_string

let max_sum grid =
  let module Cache_key = struct
    type t =
      { upto_row : int
      ; colset : Bitset.t
      }
    [@@deriving compare, hash, sexp_of]
  end
  in
  let cache = Hashtbl.create (module Cache_key) in
  let rec loop (key : Cache_key.t) =
    if key.upto_row < 0
    then 0
    else
      Hashtbl.findi_or_add cache key ~default:(fun { upto_row; colset } ->
        Bitset.fold colset ~init:0 ~f:(fun best col ->
          let cand =
            loop { upto_row = upto_row - 1; colset = Bitset.remove colset col }
          in
          Int.max best (cand + grid.(upto_row).(col))))
  in
  loop
    { upto_row = Array.length grid - 1
    ; colset = Bitset.of_list (List.init (Array.length grid) ~f:Fn.id)
    }
;;

let main () = max_sum (grid ()) |> printf "%d\n"

let%expect_test "answer" =
  main ();
  [%expect {| 13938 |}]
;;

include (val Solution.make ~problem:(Number 345) ~main)
