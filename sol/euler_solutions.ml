open! Core

(* FIXME this is horrible... *)
let modules : (string, (module Euler.Solution.S)) List.Assoc.t =
  [ "1"       , (module Sol_001)
  ; "2"       , (module Sol_002)
  ; "3"       , (module Sol_003)
  ; "4"       , (module Sol_004)
  ; "5"       , (module Sol_005)
  ; "6"       , (module Sol_006)
  ; "7"       , (module Sol_007)
  ; "8"       , (module Sol_008)
  ; "9"       , (module Sol_009)
  ; "10"      , (module Sol_010)
  ; "10-seq"  , (module Sol_010_seq)
  ; "11"      , (module Sol_011)
  ; "12"      , (module Sol_012)
  ; "13"      , (module Sol_013)
  ; "14"      , (module Sol_014)
  ; "15"      , (module Sol_015)
  ; "16"      , (module Sol_016)
  ; "17"      , (module Sol_017)
  ; "18"      , (module Sol_018)
  ; "19"      , (module Sol_019)
  ; "20"      , (module Sol_020)
  ; "21"      , (module Sol_021)
  ; "22"      , (module Sol_022)
  ; "23"      , (module Sol_023)
  ; "24"      , (module Sol_024)
  ; "25"      , (module Sol_025)
  ; "26"      , (module Sol_026)
  ; "27"      , (module Sol_027)
  ; "28"      , (module Sol_028)
  ; "29"      , (module Sol_029)
  ; "30"      , (module Sol_030)
  ; "34"      , (module Sol_034)
  ; "35"      , (module Sol_035)
  ; "36"      , (module Sol_036)
  ; "39"      , (module Sol_039)
  ; "41"      , (module Sol_041)
  ; "42"      , (module Sol_042)
  ; "43"      , (module Sol_043)
  ; "46"      , (module Sol_046)
  ; "47"      , (module Sol_047)
  ; "48"      , (module Sol_048)
  ; "52"      , (module Sol_052)
  ; "53"      , (module Sol_053)
  ; "55"      , (module Sol_055)
  ; "56"      , (module Sol_056)
  ; "57"      , (module Sol_057)
  ; "67"      , (module Sol_067)
  ; "80"      , (module Sol_080)
  ; "131"     , (module Sol_131)
  ; "139"     , (module Sol_139)
  ; "142"     , (module Sol_142)
  ; "174"     , (module Sol_174)
  ; "205"     , (module Sol_205)
  ; "206"     , (module Sol_206)
  ; "207"     , (module Sol_207)
  ; "214"     , (module Sol_214)
  ; "267"     , (module Sol_267)
  ; "315"     , (module Sol_315)
  ; "323"     , (module Sol_323)
  ; "323-sim" , (module Sol_323_sim)
  ; "348"     , (module Sol_348)
  ; "407"     , (module Sol_407)
  ; "500"     , (module Sol_500)
  ; "504"     , (module Sol_504)
  ; "549"     , (module Sol_549)
  ]
