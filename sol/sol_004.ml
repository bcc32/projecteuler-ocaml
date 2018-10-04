open! Core
open! Import

module M = struct
  let problem = Number 4
  let is_palindrome s = String.( = ) s (String.rev s)

  let main () =
    let ans = ref 0 in
    for i = 100 to 999 do
      for j = 100 to 999 do
        let digits = i * j |> Int.to_string in
        if is_palindrome digits && i * j > !ans then ans := i * j
      done
    done;
    printf "%d\n" !ans
  ;;

  let%expect_test "answer" =
    main ();
    [%expect {| 906609 |}]
  ;;
end

include Solution.Make (M)
