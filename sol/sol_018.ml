open! Core
open! Import

let parse_triangle string =
  string
  |> String.split_lines
  |> List.map ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
;;

let propagate bot top =
  let rec iter bot top acc =
    match bot, top with
    | b1 :: b2 :: bs, t :: ts ->
      let max = t + Int.max b1 b2 in
      iter (b2 :: bs) ts (max :: acc)
    | _, [] -> acc
    | _ -> Error.failwiths "length mismatched" (bot, top) [%sexp_of: int list * int list]
  in
  iter bot top [] |> List.rev
;;

let max_sum_exn triangle =
  match List.rev triangle with
  | [] -> invalid_arg "empty triangle"
  | hd :: tl ->
    List.fold tl ~init:hd ~f:propagate
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
;;

module M = struct
  let problem = Number 18

  let main () =
    let triangle = parse_triangle Problem_018.data in
    max_sum_exn triangle |> printf "%d\n"
  ;;

  (* 1074
     0.061ms *)
end

include Solution.Make (M)
