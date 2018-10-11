open! Core
open! Import

let rec make_change total coins =
  match total, coins with
  | 0, _ -> 1
  | t, _
    when t < 0 -> 0
  | _, [] -> 0
  | t, hd :: tl -> (if hd <= t then make_change (t - hd) coins else 0) + make_change t tl
;;

module M = struct
  let problem = Number 31
  let main () = make_change 200 [ 1; 2; 5; 10; 20; 50; 100; 200 ] |> printf "%d\n"
end

include Solution.Make (M)
