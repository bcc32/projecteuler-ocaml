open! Core
open! Import
module D = Distribution.Float

module Chance_card = struct
  module T = struct
    type t =
      | No_op
      | Advance_to_go
      | Go_to_jail
      | Go_to_C1
      | Go_to_E3
      | Go_to_H2
      | Go_to_R1
      | Go_to_next_R
      | Go_to_next_U
      | Go_back_3_squares
    [@@deriving compare, quickcheck, sexp_of]

    include (val Comparator.make ~compare ~sexp_of_t)
  end

  include T

  let dist =
    D.uniform'
      (module T)
      [ No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; Advance_to_go
      ; Go_to_jail
      ; Go_to_C1
      ; Go_to_E3
      ; Go_to_H2
      ; Go_to_R1
      ; Go_to_next_R
      ; Go_to_next_R
      ; Go_to_next_U
      ; Go_back_3_squares
      ]
  ;;
end

module Community_chest_card = struct
  module T = struct
    type t =
      | No_op
      | Advance_to_go
      | Go_to_jail
    [@@deriving compare, quickcheck, sexp_of]

    include (val Comparator.make ~compare ~sexp_of_t)
  end

  include T

  let dist =
    D.uniform'
      (module T)
      [ No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; No_op
      ; Advance_to_go
      ; Go_to_jail
      ]
  ;;
end

module Square : sig
  type t [@@deriving compare, enumerate, quickcheck, sexp]

  include Comparator.S with type t := t

  val index : t -> int
  val move_and_maybe_go_to_jail : t -> (int, _) D.t -> (t, comparator_witness) D.t
  val is_chance : t -> bool
  val is_community_chest : t -> bool
  val apply_chance : t -> Chance_card.t -> t
  val apply_community_chest : t -> Community_chest_card.t -> t
end = struct
  module T = struct
    type t =
      | Go
      | Go_to_jail
      | Community_chest of int
      | Chance of int
      | Jail
      | Free_parking
      | A of int
      | B of int
      | C of int
      | D of int
      | E of int
      | F of int
      | G of int
      | H of int
      | R of int
      | T of int
      | U of int
    [@@deriving bin_io, compare, quickcheck, sexp]

    let all =
      [ Go
      ; A 1
      ; Community_chest 1
      ; A 2
      ; T 1
      ; R 1
      ; B 1
      ; Chance 1
      ; B 2
      ; B 3
      ; Jail
      ; C 1
      ; U 1
      ; C 2
      ; C 3
      ; R 2
      ; D 1
      ; Community_chest 2
      ; D 2
      ; D 3
      ; Free_parking
      ; E 1
      ; Chance 2
      ; E 2
      ; E 3
      ; R 3
      ; F 1
      ; F 2
      ; U 2
      ; F 3
      ; Go_to_jail
      ; G 1
      ; G 2
      ; Community_chest 3
      ; G 3
      ; R 4
      ; Chance 3
      ; H 1
      ; T 2
      ; H 2
      ]
    ;;

    include (val Comparator.make ~compare ~sexp_of_t)
  end

  include T
  include Comparable.Make_using_comparator (T)

  let all_array = Array.of_list all

  module Square_map = Total_map.Make (T)

  let index =
    let index_naive t = all_array |> Array.findi_exn ~f:(fun _ x -> x = t) |> fst in
    let map = Square_map.create index_naive in
    fun t -> Total_map.find map t
  ;;

  let is_chance = function
    | Chance _ -> true
    | _ -> false
  ;;

  let is_community_chest = function
    | Community_chest _ -> true
    | _ -> false
  ;;

  let is_railway_company = function
    | R _ -> true
    | _ -> false
  ;;

  let is_utility_company = function
    | U _ -> true
    | _ -> false
  ;;

  let move_and_maybe_go_to_jail t die =
    let i = index t in
    D.map'
      (module T)
      die
      ~f:(fun n ->
        let dest = all_array.((i + n) % Array.length all_array) in
        if dest = Go_to_jail then Jail else dest)
  ;;

  let find_next t ~f =
    let i = index t in
    with_return (fun { return } ->
      for j = 0 to Array.length all_array - 1 do
        let x = all_array.((i + j) % Array.length all_array) in
        if f x then return x
      done;
      assert false)
  ;;

  let apply_chance t (card : Chance_card.t) =
    match card with
    | No_op -> t
    | Advance_to_go -> Go
    | Go_to_jail -> Jail
    | Go_to_C1 -> C 1
    | Go_to_E3 -> E 3
    | Go_to_H2 -> H 2
    | Go_to_R1 -> R 1
    | Go_to_next_R -> find_next t ~f:is_railway_company
    | Go_to_next_U -> find_next t ~f:is_utility_company
    | Go_back_3_squares ->
      let i = index t in
      all_array.((i - 3) % Array.length all_array)
  ;;

  let apply_community_chest t (card : Community_chest_card.t) =
    match card with
    | No_op -> t
    | Advance_to_go -> Go
    | Go_to_jail -> Jail
  ;;
end

module Game_state_dist : sig
  type t

  val init : t
  val advance : t -> die:(int, _) D.t -> t
  val to_string_modal : t -> string
  val sexp_of_square_dist : t -> Sexp.t
end = struct
  (* We pretend CH and CC distributions are uncorrelated with square; should
     probably give the same result but (shrug). *)
  type t =
    { square : (Square.t, Square.comparator_witness) D.t
        (* chance and community_chest are stored in order of the top of the stack to
           the bottom *)
    ; chance : (Chance_card.t, Chance_card.comparator_witness) D.t list
    ; community_chest :
        (Community_chest_card.t, Community_chest_card.comparator_witness) D.t list
    }

  let init =
    { square = D.uniform' (module Square) Square.all
    ; chance = List.init 16 ~f:(fun _ -> Chance_card.dist)
    ; community_chest = List.init 16 ~f:(fun _ -> Community_chest_card.dist)
    }
  ;;

  let flip_card list ~p_flip =
    let rotated =
      match list with
      | [] -> invalid_arg "flip_card"
      | hd :: tl -> tl @ [ hd ]
    in
    List.map2_exn list rotated ~f:(fun x y -> D.combine ~d1:y ~d2:x ~p1:p_flip)
  ;;

  let advance t ~die =
    let open D.Let_syntax in
    let square =
      let%bind sq = t.square in
      Square.move_and_maybe_go_to_jail sq die
    in
    let square =
      match%bind square with
      | sq when Square.is_chance sq ->
        D.map'
          (module Square)
          (List.hd_exn t.chance)
          ~f:(fun card -> Square.apply_chance sq card)
      | sq when Square.is_community_chest sq ->
        D.map'
          (module Square)
          (List.hd_exn t.community_chest)
          ~f:(fun card -> Square.apply_community_chest sq card)
      | s -> return (module Square) s
    in
    let chance =
      let p_chance =
        square |> D.map' (module Bool) ~f:Square.is_chance |> Fn.flip D.find_or_zero true
      in
      flip_card t.chance ~p_flip:p_chance
    in
    let community_chest =
      let p_community_chest =
        square
        |> D.map' (module Bool) ~f:Square.is_community_chest
        |> Fn.flip D.find_or_zero true
      in
      flip_card t.community_chest ~p_flip:p_community_chest
    in
    { square; chance; community_chest }
  ;;

  let to_string_modal t =
    t.square
    |> D.to_alist
    |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
    |> Fn.flip List.take 3
    |> List.map ~f:(fun (sq, _) -> sprintf "%02d" (Square.index sq))
    |> String.concat
  ;;

  let sexp_of_square_dist t = [%sexp (t.square : D.M(Square).t)]
end

let main () =
  let g = ref Game_state_dist.init in
  let die =
    let one_die = D.uniform' (module Int) [ 1; 2; 3; 4 ] in
    D.map2 one_die one_die ~f:(fun a b -> a + b)
  in
  for _ = 1 to 100 do
    g := Game_state_dist.advance !g ~die
  done;
  if debug then Debug.eprint_s (Game_state_dist.sexp_of_square_dist !g);
  printf !"%{Game_state_dist#modal}\n" !g
;;

(* 18.507ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 101524 |}]
;;

include (val Solution.make ~problem:(Number 84) ~main)
