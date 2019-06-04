open! Core
open! Import

module Hit : sig
  type t [@@deriving compare, hash, enumerate, sexp]

  val score : t -> int
  val is_double : t -> bool
end = struct
  module T = struct
    module For_enumerate = struct
      let all_of_int =
        [ 20; 1; 18; 4; 13; 6; 10; 15; 2; 17; 3; 19; 7; 16; 8; 11; 14; 9; 12; 5 ]
      ;;
    end

    open For_enumerate

    type t =
      | Single of int
      | Double of int
      | Triple of int
      | Bulls_eye
      | Double_bulls_eye
    [@@deriving compare, hash, enumerate, sexp]
  end

  include T

  let score t =
    match t with
    | Single n -> n
    | Double n -> 2 * n
    | Triple n -> 3 * n
    | Bulls_eye -> 25
    | Double_bulls_eye -> 50
  ;;

  let is_double t =
    match t with
    | Double _ | Double_bulls_eye -> true
    | _ -> false
  ;;
end

module Hit_or_miss : sig
  type t [@@deriving compare, enumerate, sexp_of]

  val score : t -> int
  val is_double : t -> bool
end = struct
  type t = Hit.t option [@@deriving compare, enumerate]

  let sexp_of_t = function
    | None -> Sexp.Atom "Miss"
    | Some hit -> Sexp.List [ Sexp.Atom "Hit"; [%sexp (hit : Hit.t)] ]
  ;;

  let score = Option.value_map ~default:0 ~f:Hit.score
  let is_double = Option.value_map ~default:false ~f:Hit.is_double
end

let count_checkouts ~f =
  let count = ref 0 in
  List.iter Hit_or_miss.all ~f:(fun last ->
    if Hit_or_miss.is_double last
    then
      List.iter Hit_or_miss.all ~f:(fun second ->
        List.iter Hit_or_miss.all ~f:(fun first ->
          if Hit_or_miss.compare first second <= 0
          then
            if f
                 (Hit_or_miss.score first
                  + Hit_or_miss.score second
                  + Hit_or_miss.score last)
            then incr count)));
  !count
;;

let%expect_test _ =
  print_s [%sexp (count_checkouts ~f:(Int.( = ) 6) : int)];
  [%expect {| 11 |}]
;;

let problem = Number 109
let main () = count_checkouts ~f:(fun x -> x < 100) |> printf "%d\n"

(* 38182
   1.895ms *)

include (val Solution.make ~problem ~main)
