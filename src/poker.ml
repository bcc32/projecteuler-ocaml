open! Core
open! Import

module Rank = struct
  module T = struct
    type t =
      | Two
      | Three
      | Four
      | Five
      | Six
      | Seven
      | Eight
      | Nine
      | Ten
      | Jack
      | Queen
      | King
      | Ace
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_string = function
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "T"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
  ;;

  let of_string = function
    | "2" -> Two
    | "3" -> Three
    | "4" -> Four
    | "5" -> Five
    | "6" -> Six
    | "7" -> Seven
    | "8" -> Eight
    | "9" -> Nine
    | "T" -> Ten
    | "J" -> Jack
    | "Q" -> Queen
    | "K" -> King
    | "A" -> Ace
    | rank -> raise_s [%message "invalid rank" (rank : string)]
  ;;

  let pred = function
    | Two -> None
    | Three -> Some Two
    | Four -> Some Three
    | Five -> Some Four
    | Six -> Some Five
    | Seven -> Some Six
    | Eight -> Some Seven
    | Nine -> Some Eight
    | Ten -> Some Nine
    | Jack -> Some Ten
    | Queen -> Some Jack
    | King -> Some Queen
    | Ace -> Some King
  ;;

  let succ = function
    | Two -> Some Three
    | Three -> Some Four
    | Four -> Some Five
    | Five -> Some Six
    | Six -> Some Seven
    | Seven -> Some Eight
    | Eight -> Some Nine
    | Nine -> Some Ten
    | Ten -> Some Jack
    | Jack -> Some Queen
    | Queen -> Some King
    | King -> Some Ace
    | Ace -> None
  ;;
end

module Suit = struct
  module T = struct
    type t =
      | Clubs
      | Diamonds
      | Hearts
      | Spades
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_string = function
    | Clubs -> "C"
    | Diamonds -> "D"
    | Hearts -> "H"
    | Spades -> "S"
  ;;

  let of_string = function
    | "C" -> Clubs
    | "D" -> Diamonds
    | "H" -> Hearts
    | "S" -> Spades
    | suit -> raise_s [%message "invalid suit" (suit : string)]
  ;;
end

module Card = struct
  module T = struct
    type t =
      { rank : Rank.t
      ; suit : Suit.t
      }
    [@@deriving compare, fields, hash, sexp]
  end

  include T

  let to_string { rank; suit } = Rank.to_string rank ^ Suit.to_string suit

  let of_string string =
    if String.length string <> 2
    then raise_s [%message "wrong length" ~expected:(2 : int) (string : string)];
    { rank = Rank.of_string (String.sub string ~pos:0 ~len:1)
    ; suit = Suit.of_string (String.sub string ~pos:1 ~len:1)
    }
  ;;

  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Hand_classification = struct
  module T = struct
    type t =
      | High_card of Rank.t * Rank.t * Rank.t * Rank.t * Rank.t
      | One_pair of Rank.t * Rank.t * Rank.t * Rank.t
      | Two_pairs of Rank.t * Rank.t * Rank.t
      | Three_of_a_kind of Rank.t * Rank.t * Rank.t
      | Straight of Rank.t
      | Flush of Rank.t * Rank.t * Rank.t * Rank.t * Rank.t
      | Full_house of Rank.t * Rank.t
      | Four_of_a_kind of Rank.t * Rank.t
      | Straight_flush of Rank.t
      | Royal_flush
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      match t with
      | High_card _
      | One_pair _
      | Two_pairs _
      | Three_of_a_kind _
      | Straight _
      | Flush _
      | Full_house (_, _)
      | Four_of_a_kind _ | Royal_flush -> ()
      | Straight_flush r -> assert (Rank.( < ) r Rank.Ace))
  ;;
end

module Hand = struct
  type t = Card.t * Card.t * Card.t * Card.t * Card.t [@@deriving sexp]

  let of_card_list_exn = function
    | [ a; b; c; d; e ] ->
      let array = [| a; b; c; d; e |] in
      Array.sort array ~compare:Card.compare;
      array.(0), array.(1), array.(2), array.(3), array.(4)
    | cards ->
      raise_s
        [%message "wrong number of cards" ~expected:(5 : int) (cards : Card.t list)]
  ;;

  let classify ((a, b, c, d, e) as t : t) : Hand_classification.t =
    let is_straight =
      let ( = ) = [%compare.equal: Rank.t option] in
      Some b.rank = Rank.succ a.rank
      && Some c.rank = Rank.succ b.rank
      && Some d.rank = Rank.succ c.rank
      && Some e.rank = Rank.succ d.rank
    in
    let is_flush =
      let ( = ) = Suit.( = ) in
      a.suit = b.suit && a.suit = c.suit && a.suit = d.suit && a.suit = e.suit
    in
    (* sorted first by count descending, then by rank descending *)
    let rank_counts =
      let counts = Rank.Table.create () in
      [ a; b; c; d; e ] |> List.iter ~f:(fun card -> Hashtbl.incr counts card.rank);
      Hashtbl.to_alist counts
      |> List.sort
           ~compare:
             (Comparable.lexicographic
                [ Comparable.lift Int.descending ~f:snd
                ; Comparable.lift Rank.descending ~f:fst
                ])
    in
    let high_rank = e.rank in
    if is_flush && is_straight
    then if Rank.(high_rank = Ace) then Royal_flush else Straight_flush high_rank
    else (
      match rank_counts with
      | [ (quadruple, 4); (single, 1) ] -> Four_of_a_kind (quadruple, single)
      | [ (triple, 3); (double, 2) ] -> Full_house (triple, double)
      | _
        when is_flush -> Flush (e.rank, d.rank, c.rank, b.rank, a.rank)
      | _
        when is_straight -> Straight high_rank
      | [ (triple, 3); (single_hi, 1); (single_lo, 1) ] ->
        Three_of_a_kind (triple, single_hi, single_lo)
      | [ (double_hi, 2); (double_lo, 2); (single, 1) ] ->
        Two_pairs (double_hi, double_lo, single)
      | [ (double, 2); (single_hi, 1); (single_md, 1); (single_lo, 1) ] ->
        One_pair (double, single_hi, single_md, single_lo)
      | [ (e, 1); (d, 1); (c, 1); (b, 1); (a, 1) ] -> High_card (e, d, c, b, a)
      | _ -> failwithp [%here] "bug" t [%sexp_of: t])
  ;;

  (* Examples taken from https://projecteuler.net/problem=54 *)
  let%expect_test "classify" =
    let show_classify cards =
      let classification =
        cards
        |> String.split ~on:' '
        |> List.map ~f:Card.of_string
        |> of_card_list_exn
        |> classify
      in
      print_s [%message "" (cards : string) (classification : Hand_classification.t)]
    in
    [ "5H 5C 6S 7S KD"
    ; "2C 3S 8S 8D TD"
    ; "5D 8C 9S JS AC"
    ; "2C 5C 7D 8S QH"
    ; "2D 9C AS AH AC"
    ; "3D 6D 7D TD QD"
    ; "4D 6S 9H QH QC"
    ; "3D 6D 7H QD QS"
    ; "2H 2D 4C 4D 4S"
    ; "3C 3D 3S 9S 9D"
    ]
    |> List.iter ~f:show_classify;
    [%expect
      {|
      ((cards "5H 5C 6S 7S KD") (classification (One_pair Five King Seven Six)))
      ((cards "2C 3S 8S 8D TD") (classification (One_pair Eight Ten Three Two)))
      ((cards "5D 8C 9S JS AC")
       (classification (High_card Ace Jack Nine Eight Five)))
      ((cards "2C 5C 7D 8S QH")
       (classification (High_card Queen Eight Seven Five Two)))
      ((cards "2D 9C AS AH AC") (classification (Three_of_a_kind Ace Nine Two)))
      ((cards "3D 6D 7D TD QD") (classification (Flush Queen Ten Seven Six Three)))
      ((cards "4D 6S 9H QH QC") (classification (One_pair Queen Nine Six Four)))
      ((cards "3D 6D 7H QD QS") (classification (One_pair Queen Seven Six Three)))
      ((cards "2H 2D 4C 4D 4S") (classification (Full_house Four Two)))
      ((cards "3C 3D 3S 9S 9D") (classification (Full_house Three Nine))) |}]
  ;;
end
