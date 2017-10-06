open! Core
open Re2.Std

module M = struct
  let problem_number = 22

  let path = "data/022.txt"

  let names =
    lazy (
      let name_regexp = Re2.create_exn {|"([[:upper:]]+?)"|} in
      In_channel.with_file path ~f:(fun chan ->
        In_channel.input_all chan
        |> Re2.find_all_exn name_regexp ~sub:(`Index 1)
        |> List.sort ~cmp:String.compare
      )
    )

  let letter_score ch =
    Char.to_int ch - Char.to_int 'A' + 1

  let name_score name =
    String.to_list name
    |> List.sum (module Int) ~f:letter_score

  let main () =
    let names = force names in
    List.mapi names ~f:(fun i name ->
      name_score name * (i + 1)
    )
    |> List.fold ~init:0 ~f:(+)
    |> printf "%d\n"
end

include Euler.Solution.Make(M)
