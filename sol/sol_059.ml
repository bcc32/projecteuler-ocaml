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

let iter_keys ~f =
  let key = Bytes.create 3 in
  List.iter Char.all ~f:(fun c ->
    Bytes.unsafe_set key 0 c;
    List.iter Char.all ~f:(fun c ->
      Bytes.unsafe_set key 1 c;
      List.iter Char.all ~f:(fun c ->
        Bytes.unsafe_set key 2 c;
        f (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:key))))
;;

let count_english =
  let is_english_char char = Char.is_alpha char || Char.is_whitespace char in
  String.count ~f:is_english_char
;;

module M = struct
  let problem = Number 59

  let main () =
    let ciphertext =
      Problem_059.data
      |> Parse.comma_separated_integers
      |> List.map ~f:Char.of_int_exn
      |> Bytes.of_char_list
    in
    let max_english = ref 0 in
    let max_english_plaintext = ref "" in
    iter_keys ~f:(fun key ->
      let plaintext = decrypt ciphertext ~key in
      let english_chars = count_english plaintext in
      if english_chars > !max_english
      then (
        max_english := english_chars;
        max_english_plaintext := plaintext));
    if debug then Debug.eprint_s [%sexp (!max_english_plaintext : string)];
    !max_english_plaintext |> String.sum (module Int) ~f:Char.to_int |> printf "%d\n"
  ;;

  (* 107359
     6.52568m *)
end

include Solution.Make (M)
