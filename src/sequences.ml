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
    | hd :: tl when equal x hd -> count tl x (c + 1) ac
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
        Yield { value = next; state = next })
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

let find_cycle' sequence ~equal =
  with_return (fun { return } ->
    let lambda =
      Sequence.fold_until
        (Sequence.tl_eagerly_exn sequence)
        ~init:(Sequence.hd_exn sequence, 1, 1)
        ~f:(fun (tortoise, power, lambda) hare ->
          if equal tortoise hare
          then Stop lambda
          else if power = lambda
          then Continue (hare, power * 2, 1)
          else Continue (tortoise, power, lambda + 1))
        ~finish:(fun _ ->
          (* Reached the end of the sequence without finding a cycle. *)
          return None)
    in
    let mu =
      Sequence.zip sequence (Sequence.drop_eagerly sequence lambda)
      |> Sequence.take_while ~f:(fun (tortoise, hare) -> not (equal tortoise hare))
      |> Sequence.length
    in
    Some (lambda, mu))
;;

let find_cycle ~start ~f ~equal =
  (* We should never reach the end of this infinite sequence. *)
  find_cycle'
    ~equal
    (Sequence.unfold_step ~init:start ~f:(fun x -> Yield { value = x; state = f x }))
  |> Option.value_exn
;;
