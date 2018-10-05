open! Core
open! Import

module M = struct
  let problem = `Number 8

  let doit str =
    Sequence.range 0 (String.length str - 12)
    |> Sequence.map ~f:(fun i ->
      String.sub str ~pos:i ~len:13
      |> Sequences.digits_of_string
      |> List.fold ~init:1 ~f:( * ))
    |> Sequence.max_elt ~compare:Int.compare
    |> Option.value_exn
  ;;

  let path = "data/008.txt"
  let number = lazy (In_channel.with_file path ~f:In_channel.input_all |> String.strip)
  let main () = doit (force number) |> printf "%d\n"

  (* 23514624000
     3.3ms *)
end

include Solution.Make (M)
