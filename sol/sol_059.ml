open! Core
open! Import

(* TODO Replace [Bytes.unsafe_{set,get}] with infix notation. *)

let decrypt ciphertext ~key =
  let plaintext = Bytes.copy ciphertext in
  let key_length = String.length key in
  for i = 0 to Bytes.length plaintext - 1 do
    Bytes.unsafe_set
      plaintext
      i
      (Char.unsafe_of_int
         (Char.to_int (Bytes.unsafe_get plaintext i)
          lxor Char.to_int key.[i mod key_length]))
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:plaintext
;;

let key_length = 3
let ( lxor ) a b = Char.unsafe_of_int (Char.to_int a lxor Char.to_int b)
let is_english_char char = Char.is_alpha char || Char.is_whitespace char

let best_key_char ~key_index ~ciphertext =
  let ciphertext =
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:ciphertext
  in
  List.map Char.all ~f:(fun c ->
    ( c
    , String.foldi ciphertext ~init:0 ~f:(fun i ac x ->
        if i mod key_length = key_index && is_english_char (c lxor x)
        then ac + 1
        else ac) ))
  |> List.max_elt ~compare:[%compare: _ * int]
  |> uw
  |> fst
;;

module M = struct
  let problem = Number 59

  let main () =
    let ciphertext =
      Problem_059.data
      |> Parse.csv_line ~f:Int.of_string
      |> List.map ~f:Char.of_int_exn
      |> Bytes.of_char_list
    in
    let a = best_key_char ~ciphertext ~key_index:0 in
    let b = best_key_char ~ciphertext ~key_index:1 in
    let c = best_key_char ~ciphertext ~key_index:2 in
    let plaintext = decrypt ciphertext ~key:(String.of_char_list [ a; b; c ]) in
    if debug then Debug.eprint_s [%sexp (plaintext : string)];
    plaintext |> String.sum (module Int) ~f:Char.to_int |> printf "%d\n"
  ;;

  (* 7.376ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 107359 |}]
  ;;
end

include Solution.Make (M)
