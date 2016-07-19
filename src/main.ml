open Core.Std

let command =
  Command.group ~summary:"Run ProjectEuler solutions"
    ~preserve_subcommand_order:()
    [ "1"     , Sol_001     .command
    ; "2"     , Sol_002     .command
    ; "3"     , Sol_003     .command
    ; "4"     , Sol_004     .command
    ; "5"     , Sol_005     .command
    ; "6"     , Sol_006     .command
    ; "7"     , Sol_007     .command
    ; "8"     , Sol_008     .command
    ; "9"     , Sol_009     .command
    ; "10"    , Sol_010     .command
    ; "10-alt", Sol_010_alt .command
    ; "10-mut", Sol_010_mut .command
    ; "12"    , Sol_012     .command
    ; "14"    , Sol_014     .command
    ; "16"    , Sol_016     .command
    ; "20"    , Sol_020     .command
    ; "21"    , Sol_021     .command
    ; "25"    , Sol_025     .command
    ]

let () =
  Command.run ~version:"0.0.1" command
