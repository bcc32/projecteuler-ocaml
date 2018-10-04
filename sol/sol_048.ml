open! Core
open! Import

module M = struct
  let problem = Number 48

  module Bigint = struct
    include Bigint

    let modulus =
      let ten = Bigint.of_int 10 in
      Bigint.pow ten ten
    ;;

    let ( + ) a b = (a + b) % modulus
  end

  let main () =
    let sum =
      Sequence.range ~stop:`inclusive 1 1000
      |> Sequence.sum
           (module Bigint)
           ~f:(fun n ->
             let n = Bigint.of_int n in
             Bigint.pow n n)
    in
    sum |> printf !"%{Bigint}\n"
  ;;

  (* 4.026ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 9110846700 |}]
  ;;
end

include Solution.Make (M)
