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
    ; "11"    , Sol_011     .command
    ; "12"    , Sol_012     .command
    ; "13"    , Sol_013     .command
    ; "14"    , Sol_014     .command
    ; "15"    , Sol_015     .command
    ; "16"    , Sol_016     .command
    ; "18"    , Sol_018     .command
    ; "19"    , Sol_019     .command
    ; "20"    , Sol_020     .command
    ; "21"    , Sol_021     .command
    ; "22"    , Sol_022     .command
    ; "23"    , Sol_023     .command
    ; "24"    , Sol_024     .command
    ; "25"    , Sol_025     .command
    ; "27"    , Sol_027     .command
    ; "29"    , Sol_029     .command
    ; "30"    , Sol_030     .command
    ; "34"    , Sol_034     .command
    ; "35"    , Sol_035     .command
    ; "36"    , Sol_036     .command
    ; "48"    , Sol_048     .command
    ; "67"    , Sol_067     .command
    ]

let () =
  Command.run ~version:"0.0.1" command
