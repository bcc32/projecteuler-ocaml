open! Core

type solution_id =
  [ `Number of int
  | `Custom of int * [ `Key of string ] * [ `Description of string ] ]

module type Solution = sig
  val problem : solution_id
  val main : unit -> unit
end

module type S = sig
  val command : Command.t
  val command_name : string
end

let time_unit f () =
  let start = Time.now () in
  protect ~f ~finally:(fun () ->
    let finish = Time.now () in
    Time.diff finish start
    |> Time.Span.to_short_string
    |> print_endline)
;;

module Make (M : Solution) : S = struct
  let command =
    let main =
      let open Command.Let_syntax in
      let%map_open
        time = flag "-time" no_arg ~doc:" measure and print runtime"
      in
      fun () ->
        if time
        then time_unit M.main ()
        else M.main ()
    in
    let summary =
      match M.problem with
      | `Number n -> sprintf "Problem %d" n
      | `Custom (n, `Key _, `Description d) -> sprintf "Problem %d (%s)" n d
    in
    Command.basic' main ~summary
  ;;

  let command_name =
    match M.problem with
    | `Number n -> Int.to_string n
    | `Custom (n, `Key k, `Description _) -> sprintf "%d-%s" n k
  ;;
end
