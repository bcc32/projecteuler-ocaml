open! Core

(* FIXME this module needs a better name *)

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

let next_permutation ~cmp a =
  let a = Array.copy a in
  with_return_option (fun { return } ->
    for i = Array.length a - 2 downto 0 do
      if cmp a.(i) a.(i + 1) < 0
      then (
        let min_index = ref (i + 1) in
        for j = i + 2 to Array.length a - 1 do
          if cmp a.(i) a.(j) < 0 && cmp a.(j) a.(!min_index) < 0
          then min_index := j
        done;
        Array.swap a i !min_index;
        Array.sort ~pos:(i + 1) ~cmp a;
        return a)
    done)
;;

let permutations ~cmp l =
  let a = Array.sorted_copy ~cmp (List.to_array l) in
  let next_permutations =
    Sequence.unfold ~init:a ~f:(fun a ->
      let open Option.Let_syntax in
      let%map next = next_permutation ~cmp a in
      (Array.to_list next, next))
  in
  Sequence.shift_right next_permutations (Array.to_list a)
;;
