open! Core
open! Import

let digits_of_string n = String.to_list_rev n |> List.rev_map ~f:Char.get_digit_exn

let run_length_encode lst ~equal =
  let rec start lst ac =
    match lst with
    | [] -> ac
    | hd :: tl -> count tl hd 1 ac
  and count lst x c ac =
    match lst with
    | hd :: tl
      when equal x hd -> count tl x (c + 1) ac
    | _ -> start lst ((x, c) :: ac)
  in
  List.rev (start lst [])
;;

let run_length_decode = List.concat_map ~f:(fun (x, n) -> List.init n ~f:(fun _ -> x))

let%expect_test _ =
  let aabbbbc = run_length_decode [ "a", 2; "b", 4; "c", 1 ] in
  print_s [%sexp (aabbbbc : string list)];
  [%expect {| (a a b b b b c) |}]
;;

let is_palindrome (type a) (module M : Equal.S with type t = a) l =
  [%equal: M.t list] l (List.rev l)
;;

let next_permutation_inplace a ~compare =
  let ( <<< ) i j = compare a.(i) a.(j) < 0 in
  with_return (fun { return } ->
    for i = Array.length a - 2 downto 0 do
      if i <<< i + 1
      then (
        let min_index = ref (i + 1) in
        for j = i + 2 to Array.length a - 1 do
          if i <<< j && j <<< !min_index then min_index := j
        done;
        Array.swap a i !min_index;
        Array.sort ~pos:(i + 1) ~compare a;
        return true)
    done;
    false)
;;

let prev_permutation_inplace a ~compare =
  next_permutation_inplace a ~compare:(fun a b -> compare b a)
;;

let permutations list ~compare =
  let init = List.sort list ~compare in
  let next_permutations =
    Sequence.unfold_step ~init ~f:(fun list ->
      let a = Array.of_list list in
      if next_permutation_inplace a ~compare
      then (
        let next = Array.to_list a in
        Yield (next, next))
      else Done)
  in
  Sequence.shift_right next_permutations init
;;

let iter_permutations list ~compare ~f =
  let sequence = Array.of_list list in
  Array.sort sequence ~compare;
  let rec loop () =
    f (Array.Permissioned.of_array_id sequence);
    if next_permutation_inplace sequence ~compare then loop ()
  in
  loop ()
;;
