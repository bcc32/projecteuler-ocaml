open! Core
open! Import

module D = Distribution.Make(Float)

let gen_prob = Float.gen_incl 0. 1.

let%test_unit "singleton" =
  let d = D.singleton 0 in
  [%test_result: float] ~expect:1. (D.find_exn d 0)
;;

let%test_unit "scale" =
  Quickcheck.test gen_prob ~f:(fun p ->
    let d = D.scale (D.singleton 0) p in
    [%test_result: float] ~expect:p (D.find_exn d 0))
;;

let%test_unit "combine" =
  let d1 = D.singleton 1 in
  let d2 = D.singleton 2 in
  Quickcheck.test gen_prob ~f:(fun p1 ->
    let d = D.combine ~d1 ~d2 ~p1 in
    [%test_result: float] (D.find_exn d 1) ~expect:p1;
    [%test_result: float] (D.find_exn d 2) ~expect:(1. -. p1))
;;

let%test_unit "monad laws" =
  let f x = D.singleton (x + 1) in
  let g x = D.combine ~p1:0.5 ~d1:(D.singleton x) ~d2:(D.singleton (-x)) in
  let t = D.singleton 0 in
  Quickcheck.test Int.gen ~f:(fun v ->
    let open D.Let_syntax in
    [%test_result: int D.t] (return v >>= f) ~expect:(f v);
    [%test_result: int D.t] (t >>= return) ~expect:t;
    [%test_result: int D.t] (t >>= f >>= g)
      ~expect:(t >>= (fun x -> f x >>= g)))
;;
