open! Core

module M = struct
  let problem_number = 11

  let path = "data/011.txt"
  let size = 20
  let product_size = 4

  let grid =
    lazy (
      In_channel.with_file path ~f:(fun chan ->
        In_channel.input_lines chan
        |> List.map ~f:(fun line ->
          String.split line ~on:' '
          |> List.map ~f:Int.of_string
          |> List.to_array)
        |> List.to_array))
  ;;

  let horizontal_products grid =
    Array.to_sequence grid
    |> Sequence.map ~f:(fun row ->
      Sequence.range ~stop:`inclusive 0 (size - product_size)
      |> Sequence.map ~f:(fun i ->
        Sequence.range i (i + product_size)
        |> Sequence.map ~f:(fun i -> row.(i))
        |> Sequence.fold ~init:1 ~f:( * )))
    |> Sequence.concat
  ;;

  let vertical_products grid =
    horizontal_products (Array.transpose_exn grid)

  let lr_diagonal_products grid =
    let indices = Sequence.range ~stop:`inclusive 0 (size - product_size) in
    Sequence.cartesian_product indices indices
    |> Sequence.map ~f:(fun (i, j) ->
      Sequence.range 0 product_size
      |> Sequence.map ~f:(fun x -> grid.(i + x).(j + x))
      |> Sequence.fold ~init:1 ~f:( * ))
  ;;

  let rl_diagonal_products grid =
    let grid = Array.copy grid in
    Array.rev_inplace grid;
    lr_diagonal_products grid

  let main () =
    let grid = force grid in
    [ horizontal_products grid
    ; vertical_products grid
    ; lr_diagonal_products grid
    ; rl_diagonal_products grid ]
    |> Sequence.of_list
    |> Sequence.concat
    |> Sequence.max_elt ~cmp:Int.compare
    |> Option.value_exn
    |> printf "%d\n"
  ;;
end

include Euler.Solution.Make(M)
