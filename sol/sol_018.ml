open! Core

module M = struct
  let problem_number = 18

  let path = "data/018.txt"

  let read_triangle path =
    In_channel.with_file path ~f:(fun chan ->
      In_channel.input_lines chan
      |> List.map ~f:(fun line ->
        String.split line ~on:' '
        |> List.map ~f:Int.of_string
      )
    )

  let propagate bot top =
    let rec iter bot top acc =
      match bot, top with
      | (b1 :: b2 :: bs), (t :: ts) ->
        let max = t + Int.max b1 b2 in
        iter (b2 :: bs) ts (max :: acc)
      | _, [] -> acc
      | _ -> Error.failwiths "length mismatched" (bot, top)
               [%sexp_of: int list * int list]
    in
    iter bot top []
    |> List.rev

  let max_sum_exn triangle =
    match List.rev triangle with
    | [] -> invalid_arg "empty triangle"
    | hd :: tl ->
      List.fold tl ~init:hd ~f:propagate
      |> List.max_elt ~cmp:Int.compare
      |> Option.value_exn

  let main () =
    let triangle = read_triangle path in
    max_sum_exn triangle
    |> printf "%d\n"
end

include Euler.Solution.Make(M)

let read_triangle = M.read_triangle
let max_sum_exn   = M.max_sum_exn
