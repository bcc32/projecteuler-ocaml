open! Core

(* FIXME this module needs a better name *)

let digits_of_string n =
  let zero = Char.to_int '0' in
  String.to_list_rev n
  |> List.rev_map ~f:(fun c -> Char.to_int c - zero)
;;

let run_length_encode lst ~equal =
  let rec start lst ac =
    match lst with
    | [] -> ac
    | hd :: tl -> count tl hd 1 ac
  and count lst x c ac =
    match lst with
    | hd :: tl when equal x hd -> count tl x (c + 1) ac
    | _ -> start lst ((x, c) :: ac)
  in
  List.rev (start lst [])
;;

let is_palindrome l ~equal = List.equal ~equal l (List.rev l)

let next_permutation_inplace ~cmp a =
  let (<<<) i j = cmp a.(i) a.(j) < 0 in
  with_return (fun { return } ->
    for i = Array.length a - 2 downto 0 do
      if i <<< i + 1
      then (
        let min_index = ref (i + 1) in
        for j = i + 2 to Array.length a - 1 do
          if i <<< j && j <<< !min_index
          then min_index := j
        done;
        Array.swap a i !min_index;
        Array.sort ~pos:(i + 1) ~cmp a;
        return true)
    done;
    false)
;;

let prev_permutation_inplace ~cmp a =
  next_permutation_inplace a ~cmp:(Fn.flip cmp)
;;

let permutations ~cmp l =
  let init = List.sort l ~cmp in
  let next_permutations =
    Sequence.unfold ~init ~f:(fun list ->
      let a = Array.of_list list in
      if next_permutation_inplace a ~cmp
      then (
        let next = Array.to_list a in
        Some (next, next))
      else None)
  in
  Sequence.shift_right next_permutations init
;;
