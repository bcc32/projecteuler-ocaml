open! Core
open! Import

module M = struct
  let problem = Number 42
  let path = "data/042.txt"

  let words =
    lazy
      (let word_regexp =
         let open Re in
         compile (seq [ char '"'; group (non_greedy (rep1 upper)); char '"' ])
       in
       In_channel.with_file path ~f:(fun chan ->
         In_channel.input_all chan
         |> Re.all word_regexp
         |> List.map ~f:(fun groups -> Re.get groups 1)))
  ;;

  let word_value word =
    let letter_value letter = Char.to_int letter - Char.to_int 'A' + 1 in
    String.to_list word |> List.sum (module Int) ~f:letter_value
  ;;

  let is_triangle_number =
    let cache = Int.Table.create () in
    fun t ->
      Hashtbl.find_or_add cache t ~default:(fun () ->
        let n = Float.(sqrt (of_int t * 2.0) |> to_int) in
        t = n * (n + 1) / 2)
  ;;

  let main () =
    force words
    |> List.map ~f:word_value
    |> List.count ~f:is_triangle_number
    |> printf "%d\n"
  ;;

  (* 162
     1.6ms *)
end

include Solution.Make (M)
