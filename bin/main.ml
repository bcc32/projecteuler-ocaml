open! Core
open! Import

let main =
  Cmd.group
    (Cmd.info "euler" ~doc:"Run ProjectEuler solutions" ~version:"%%VERSION%%")
    ~default:Term.(ret (const (`Help (`Auto, None))))
    [ Solutions.list_command; Solutions.run_command ]
;;

let () = Cmd.eval main |> exit
