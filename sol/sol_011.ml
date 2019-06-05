open! Core
open! Import

let size = 20
let product_size = 4
let grid = lazy (Problem_011.data |> Parse.space_separated_grid ~conv:Int.of_string)

let horizontal_products grid =
  let open Sequence.Let_syntax in
  let%bind row = Array.to_sequence grid in
  let%map i = Sequence.range ~stop:`inclusive 0 (size - product_size) in
  Sequence.fold
    ~init:1
    ~f:( * )
    (let%map j = Sequence.range i (i + product_size) in
     row.(j))
;;

let vertical_products grid = horizontal_products (Array.transpose_exn grid)

let lr_diagonal_products grid =
  let open Sequence.Let_syntax in
  let indices = Sequence.range ~stop:`inclusive 0 (size - product_size) in
  let%bind i = indices in
  let%map j = indices in
  Sequence.fold
    ~init:1
    ~f:( * )
    (let%map x = Sequence.range 0 product_size in
     grid.(i + x).(j + x))
;;

let rl_diagonal_products grid =
  let grid = Array.copy grid in
  Array.rev_inplace grid;
  lr_diagonal_products grid
;;

let main () =
  let grid = force grid in
  [ horizontal_products grid
  ; vertical_products grid
  ; lr_diagonal_products grid
  ; rl_diagonal_products grid
  ]
  |> Sequence.of_list
  |> Sequence.concat
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> printf "%d\n"
;;

(* 148.681us *)
let%expect_test "answer" =
  main ();
  [%expect {| 70600674 |}]
;;

include (val Solution.make ~problem:(Number 11) ~main)
