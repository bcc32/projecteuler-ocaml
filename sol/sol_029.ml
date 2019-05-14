open! Core
open! Import

module M = struct
  let problem = Number 29

  let main () =
    let powers = Hash_set.create (module Bigint) () in
    for a = 2 to 100 do
      for b = 2 to 100 do
        let power = Bigint.pow (Bigint.of_int a) (Bigint.of_int b) in
        Hash_set.add powers power
      done
    done;
    printf "%d\n" (Hash_set.length powers)
  ;;

  (* 5.239ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 9183 |}]
  ;;
end

include Solution.Make (M)
