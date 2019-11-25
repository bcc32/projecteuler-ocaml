open! Core
open! Import

let main =
  Term.(
    ( ret (const (`Help (`Auto, None)))
    , info "euler" ~doc:"Run ProjectEuler solutions" ~version:"%%VERSION%%" ))
;;

let () = Term.(eval_choice main [ Solutions.list_command; Solutions.run_command ] |> exit)
