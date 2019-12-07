open! Core
open! Import

let ways_to_replace =
  Memo.recursive
    (module Int)
    (fun ways_to_replace total_size ->
       if total_size < 0
       then 0
       else if total_size = 0
       then 1
       else (
         let gray = ways_to_replace (total_size - 1) in
         let red = ways_to_replace (total_size - 2) in
         let green = ways_to_replace (total_size - 3) in
         let blue = ways_to_replace (total_size - 4) in
         gray + red + green + blue))
;;

let main () = printf "%d\n" (ways_to_replace 50)

let%expect_test "answer" =
  main ();
  [%expect {| 100808458960497 |}]
;;

include (val Solution.make ~problem:(Number 117) ~main)
