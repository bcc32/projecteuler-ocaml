open! Core
open! Import

let iter_base10_palindromes digits ~f =
  let limit = Int.pow 10 ((digits + 1) / 2) in
  let make_palindrome rhs =
    let b = Bytes.make digits '0' in
    let s = Int.to_string rhs in
    for si = 0 to String.length s - 1 do
      let bi_left = String.length s - 1 - si in
      let bi_right = digits - 1 - (String.length s - 1 - si) in
      (* TODO Change this to the a.[i] <- c infix syntax when it no longer
         refers to [String]. *)
      Bytes.set b bi_left s.[si];
      Bytes.set b bi_right s.[si]
    done;
    Int.of_string (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b)
  in
  for i = 0 to limit - 1 do
    if i % 10 <> 0 then f (make_palindrome i)
  done
;;

let%expect_test _ =
  let palindrome_list digits =
    let list = ref [] in
    iter_base10_palindromes digits ~f:(fun x -> list := x :: !list);
    List.rev !list
  in
  print_s [%sexp (palindrome_list 2 : int list)];
  [%expect {|
    (11 22 33 44 55 66 77 88 99) |}];
  print_s [%sexp (palindrome_list 3 : int list)];
  [%expect
    {|
    (101 202 303 404 505 606 707 808 909 111 212 313 414 515 616 717 818 919 121
     222 323 424 525 626 727 828 929 131 232 333 434 535 636 737 838 939 141 242
     343 444 545 646 747 848 949 151 252 353 454 555 656 757 858 959 161 262 363
     464 565 666 767 868 969 171 272 373 474 575 676 777 878 979 181 282 383 484
     585 686 787 888 989 191 292 393 494 595 696 797 898 999) |}]
;;

module M = struct
  let problem = Number 36

  let main () =
    let sum = ref 0 in
    for digits = 1 to 6 do
      iter_base10_palindromes digits ~f:(fun n ->
        if Sequences.is_palindrome
             ~equal:Int.equal
             (Number_theory.Int.digits_of_int ~base:2 n)
        then sum := !sum + n)
    done;
    printf "%d\n" !sum
  ;;

  (* 2.167ms
     872187 *)
end

include Solution.Make (M)
