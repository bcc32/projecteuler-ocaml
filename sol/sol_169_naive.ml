open! Core
open! Import

module Number : sig
  (** left-to-right *)
  type t [@@deriving compare, sexp_of]

  val of_bigint : Bigint.t -> t
  val n_initial : t
  val ten : t
  val iter_candidates : t -> f:(t -> unit) -> unit
  val count_candidates : t -> int
end = struct
  type digit =
    | Zero
    | One
    | Two
  [@@deriving compare]

  type t = digit list [@@deriving compare]

  let of_bigint bigint =
    let digit_of_int = function
      | 0 -> Zero
      | 1 -> One
      | _ -> assert false
    in
    let bin_of_hex hex_digit =
      let value =
        match hex_digit with
        | '0' .. '9' -> Char.get_digit_exn hex_digit
        | 'a' .. 'f' -> Char.to_int hex_digit - Char.to_int 'a' + 10
        | _ -> assert false
      in
      [ digit_of_int ((value lsr 3) land 1)
      ; digit_of_int ((value lsr 2) land 1)
      ; digit_of_int ((value lsr 1) land 1)
      ; digit_of_int ((value lsr 0) land 1)
      ]
    in
    bigint
    |> Bigint.Hex.to_string
    |> String.chop_prefix_exn ~prefix:"0x"
    |> String.to_list
    |> List.concat_map ~f:bin_of_hex
  ;;

  (* 10^25 *)
  let n_initial =
    {|1000_0100_0101_1001_0101_0001_0110
      0001_0100_0000_0001_0100_1000_0100
      1010_0000_0000_0000_0000_0000_0000|}
    |> String.to_list
    |> List.filter_map ~f:(function
      | '_' -> None
      | ws when Char.is_whitespace ws -> None
      | '0' -> Some Zero
      | '1' -> Some One
      | _ -> assert false)
  ;;

  let ten = [ One; Zero; One; Zero ]

  (* when add_from_left runs off the end of the number *)
  exception Can't_add

  let rec add_from_left t dig =
    match t, dig with
    | _, Zero -> t
    | [], _ -> Exn.raise_without_backtrace Can't_add
    | Zero :: tl, dig -> dig :: tl
    | One :: tl, One -> Two :: tl
    | One :: tl, Two -> Two :: add_from_left tl Two
    | Two :: tl, One -> Two :: add_from_left tl Two
    | Two :: _, Two -> Exn.raise_without_backtrace Can't_add
  ;;

  let rec iter_candidates t ~f =
    match t with
    | [] -> f []
    | Zero :: tl -> iter_candidates tl ~f:(fun x -> f (Zero :: x))
    | One :: tl ->
      iter_candidates tl ~f:(fun x -> f (One :: x));
      (match add_from_left tl Two with
       | carry -> iter_candidates (Zero :: carry) ~f
       | exception Can't_add -> ())
    | Two :: tl ->
      iter_candidates tl ~f:(fun x -> f (Two :: x));
      (match add_from_left tl Two with
       | carry -> iter_candidates (One :: carry) ~f
       | exception Can't_add -> ())
  ;;

  let rec count_candidates t =
    match t with
    | [] -> 1
    | Zero :: tl -> count_candidates tl
    | One :: tl ->
      count_candidates tl
      +
      (match add_from_left tl Two with
       | carry -> count_candidates carry (* equal to count of (Zero :: carry) *)
       | exception Can't_add -> 0)
    | Two :: tl ->
      count_candidates tl
      +
      (match add_from_left tl Two with
       | carry ->
         (* count_candidates (One :: carry) *)
         count_candidates carry
         +
         (match add_from_left carry Two with
          | carry -> count_candidates carry
          | exception Can't_add -> 0)
       | exception Can't_add -> 0)
  ;;

  let sexp_of_t t =
    let repr_digit_instance pow = Sexp.Atom (sprintf "2^%d" pow) in
    let len = List.length t in
    Sexp.List
      (List.concat_mapi t ~f:(fun i dig ->
         let pow = len - 1 - i in
         match dig with
         | Zero -> []
         | One -> [ repr_digit_instance pow ]
         | Two -> [ repr_digit_instance pow; repr_digit_instance pow ]))
  ;;
end

let%expect_test _ =
  Number.ten |> Number.iter_candidates ~f:(fun x -> print_s [%sexp (x : Number.t)]);
  [%expect
    {|
    (2^3 2^1)
    (2^3 2^0 2^0)
    (2^2 2^2 2^1)
    (2^2 2^2 2^0 2^0)
    (2^2 2^1 2^1 2^0 2^0) |}];
  print_s [%sexp (Number.count_candidates Number.ten : int)];
  [%expect {| 5 |}]
;;

let%expect_test "pattern" =
  for i = 1 to 10 do
    print_s
      [%sexp
        (i : Int.Hex.t)
      , (Number.count_candidates (i |> Bigint.of_int |> Number.of_bigint) : int)]
  done;
  [%expect
    {|
    (0x1 1)
    (0x2 2)
    (0x3 1)
    (0x4 3)
    (0x5 2)
    (0x6 3)
    (0x7 1)
    (0x8 4)
    (0x9 3)
    (0xa 5) |}]
;;

let%test_unit _ = [%test_eq: Number.t] Number.ten (Number.of_bigint (Bigint.of_int 10))

let%test_unit _ =
  [%test_eq: Number.t]
    Number.n_initial
    (Number.of_bigint Bigint.(pow (of_int 10) (of_int 25)))
;;

let main () = Number.n_initial |> Number.count_candidates |> printf "%d\n"

(* 178653872807
   2.09186h *)

include
  (val Solution.make
         ~problem:
           (Tagged { number = 169; tag = "naive"; description = "slow, naive counting" })
         ~main)
