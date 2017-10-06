open! Core

module type Solution = sig
  val problem_number : int
  val main : unit -> unit
end

module type S = sig
  val command : Command.t
end

module Make (M : Solution) : S = struct
  let time_unit f () =
    let start = Time.now () in
    protect ~f ~finally:(fun () ->
      let finish = Time.now () in
      Time.diff finish start
      |> Time.Span.to_short_string
      |> print_endline)

  let command =
    let summary = sprintf "Problem %d" M.problem_number in
    Command.basic' ~summary begin
      let open Command.Let_syntax in
      let%map_open
        time = flag "-time" no_arg ~doc:" measure and print runtime"
      in
      fun () ->
        if time
        then time_unit M.main ()
        else M.main ()
    end
end
