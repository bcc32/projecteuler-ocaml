open Core

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
    ; "10-seq", Sol_010_seq .command
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
    ; "39"    , Sol_039     .command
    ; "41"    , Sol_041     .command
    ; "42"    , Sol_042     .command
    ; "43"    , Sol_043     .command
    ; "46"    , Sol_046     .command
    ; "47"    , Sol_047     .command
    ; "48"    , Sol_048     .command
    ; "52"    , Sol_052     .command
    ; "53"    , Sol_053     .command
    ; "55"    , Sol_055     .command
    ; "56"    , Sol_056     .command
    ; "57"    , Sol_057     .command
    ; "67"    , Sol_067     .command
    ; "80"    , Sol_080     .command
    ; "205"   , Sol_205     .command
    ; "206"   , Sol_206     .command
    ; "207"   , Sol_207     .command
    ; "214"   , Sol_214     .command
    ; "267"   , Sol_267     .command
    ; "323"   , Sol_323     .command
    ; "bench" , Benchmark   .command
    ]

let () =
  Command.run ~version:"0.0.2" command
