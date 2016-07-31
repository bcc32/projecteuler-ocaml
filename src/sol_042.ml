open Core.Std
open Re2.Std

module M = struct
  let problem_number = 42

  let path = "data/042.txt"

  let words =
    lazy (
      let word_regexp = Re2.create_exn {|"([[:upper:]]+?)"|} in
      In_channel.with_file path ~f:(fun chan ->
        In_channel.input_all chan
        |> Re2.find_all_exn word_regexp ~sub:(`Index 1)
      )
    )

  let word_value word =
    let letter_value letter = Char.to_int letter - Char.to_int 'A' + 1 in
    String.to_list word
    |> List.sum (module Int) ~f:letter_value

  let is_triangle_number_table =
    Int.Table.create ()

  let is_triangle_number t =
    Hashtbl.find_or_add is_triangle_number_table t
      ~default:(fun () ->
        let n = Float.(sqrt (of_int t * 2.0) |> to_int) in
        t = n * (n + 1) / 2
      )

  let main () =
    let words = force words in
    words
    |> List.map ~f:word_value
    |> List.count ~f:is_triangle_number
    |> printf "%d\n"
end

include Solution.Make(M)
