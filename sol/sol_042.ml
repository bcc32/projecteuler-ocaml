open! Core
open! Import

module M = struct
  let problem = `Number 42

  let path = "data/042.txt"

  let words =
    lazy (
      let word_regexp = Re2.create_exn {|"([[:upper:]]+?)"|} in
      In_channel.with_file path ~f:(fun chan ->
        In_channel.input_all chan
        |> Re2.find_all_exn word_regexp ~sub:(`Index 1)))
  ;;

  let word_value word =
    let letter_value letter = Char.to_int letter - Char.to_int 'A' + 1 in
    String.to_list word
    |> List.sum (module Int) ~f:letter_value
  ;;

  let is_triangle_number =
    let cache = Int.Table.create () in
    fun t ->
      Hashtbl.find_or_add cache t
        ~default:(fun () ->
          let n = Float.(sqrt (of_int t * 2.0) |> to_int) in
          t = n * (n + 1) / 2)

  let main () =
    force words
    |> List.map ~f:word_value
    |> List.count ~f:is_triangle_number
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
