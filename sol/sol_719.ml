open! Core
open! Import

let sum_with_sep_mask ~n_digits ~sep_mask =
  (* place is tens power of next digit to be added to term *)
  let rec loop i ~term ~place ~sum ~mask =
    if debug
    then
      Debug.eprint_s
        [%message [%here] (i : int) (term : int) (place : int) (sum : int) (mask : int)];
    if i < 0
    then term + sum
    else if mask land 1 <> 0
    then
      (* separate *)
      loop (i - 1) ~term:n_digits.(i) ~place:10 ~sum:(term + sum) ~mask:(mask lsr 1)
    else
      (* same term *)
      loop
        (i - 1)
        ~term:((place * n_digits.(i)) + term)
        ~place:(10 * place)
        ~sum
        ~mask:(mask lsr 1)
  in
  loop
    (Array.length n_digits - 2)
    ~term:(Array.last n_digits)
    ~place:10
    ~sum:0
    ~mask:sep_mask
;;

let%expect_test "sum_with_sep_mask" =
  let n_digits = [| 1; 2; 4; 8 |] in
  let test (sep_mask, expect) =
    let sum = sum_with_sep_mask ~n_digits ~sep_mask in
    require_equal
      [%here]
      (module Int)
      sum
      expect
      ~if_false_then_print_s:(lazy [%message (sep_mask : Int.Hex.t)])
  in
  List.iter ~f:test [ 0b000, 1248; 0b111, 1 + 2 + 4 + 8; 0b010, 12 + 48 ];
  [%expect {| |}]
;;

let is_s_number ~sqrt ~n =
  let n_digits = Number_theory.Int.As_base10.to_array n in
  Sequence.range 0 (Int.pow 2 (Array.length n_digits - 1))
  |> Sequence.find_map ~f:(fun sep_mask ->
    let summed = sum_with_sep_mask ~n_digits ~sep_mask in
    if sqrt = summed then Some (sqrt, n, sep_mask) else None)
;;

let t ~max_sqrt =
  let squares =
    Sequence.range 2 max_sqrt ~stop:`inclusive |> Sequence.map ~f:(fun x -> x, x * x)
  in
  Sequence.filter_map squares ~f:(fun (sqrt, n) -> is_s_number ~sqrt ~n)
;;

let%expect_test "T(10^4)" =
  let s_numbers = t ~max_sqrt:100 in
  Sequence.sum (module Int) s_numbers ~f:(fun (_, n, _) -> n)
  |> [%sexp_of: int]
  |> print_s;
  [%expect {| 41333 |}]
;;

(* 128088830547982
   1m59.823267706s *)
let main () =
  let max_sqrt = 1_000_000 in
  let s_numbers = t ~max_sqrt in
  s_numbers
  |> Sequence.sum (module Int) ~f:(fun (_, n, _) -> n)
  |> [%sexp_of: int]
  |> print_s
;;

include (val Solution.make ~problem:(Number 719) ~main)
