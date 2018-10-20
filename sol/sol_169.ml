open! Core
open! Import

module Number : sig
  (** left-to-right *)
  type t [@@deriving sexp_of]

  val n_initial : t
  val ten : t
  val iter_candidates : t -> f:(t -> unit) -> unit
  val count_candidates : t -> int
end = struct
  type digit =
    | Zero
    | One
    | Two

  type t = digit list

  (* 10^25 *)
  let n_initial =
    {|1000_0100_0101_1001_0101_0001_0110
      0001_0100_0000_0001_0100_1000_0100
      1010_0000_0000_0000_0000_0000_0000|}
    |> String.to_list
    |> List.filter_map ~f:(function
      | '_' -> None
      | ws
        when Char.is_whitespace ws -> None
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
    | [], _ -> raise Can't_add
    | Zero :: tl, dig -> dig :: tl
    | One :: tl, One -> Two :: tl
    | One :: tl, Two -> Two :: add_from_left tl Two
    | Two :: tl, One -> Two :: add_from_left tl Two
    | Two :: _, Two -> raise Can't_add
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

module M = struct
  let problem = Number 169
  let main () = Number.n_initial |> Number.count_candidates |> printf "%d\n"

  (* 178653872807
     2.09186h *)
end

include Solution.Make (M)
