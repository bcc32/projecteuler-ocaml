open! Core
open! Import

module M = struct
  let problem = Number 22
  let path = "data/022.txt"

  let names =
    lazy
      (let name_regexp =
         let open Re in
         compile (seq [ char '"'; group (non_greedy (rep1 upper)); char '"' ])
       in
       In_channel.with_file path ~f:(fun chan ->
         In_channel.input_all chan
         |> Re.all name_regexp
         |> List.map ~f:(fun groups -> Re.get groups 1)
         |> List.sort ~compare:String.compare))
  ;;

  let letter_score ch = Char.to_int ch - Char.to_int 'A' + 1
  let name_score name = String.to_list name |> List.sum (module Int) ~f:letter_score

  let main () =
    let names = force names in
    List.mapi names ~f:(fun i name -> name_score name * (i + 1))
    |> List.fold ~init:0 ~f:( + )
    |> printf "%d\n"
  ;;

  (* 871198282
     5.4ms*)
end

include Solution.Make (M)
