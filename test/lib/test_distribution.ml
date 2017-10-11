open! Core
open! Import

module D = Distribution.Make(Float)

let gen_prob = Float.gen_incl 0. 1.

let gen_distribution =
  let open Quickcheck.Generator.Let_syntax in
  let%map keys = Int.Map.gen Int.gen gen_prob in
  keys
  |> Map.to_alist
  |> D.of_alist_exn
  |> D.normalize
;;

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

let%test_unit "bind" =
  let f x = D.combine ~p1:0.5 ~d1:(D.singleton x) ~d2:(D.singleton (x + 1)) in
  let t = D.combine ~p1:0.2 ~d1:(D.singleton 0) ~d2:(D.singleton 1) in
  let expect =
    [ ( 0, 0.1 )
    ; ( 1, 0.5 )
    ; ( 2, 0.4 ) ]
    |> D.of_alist_exn
  in
  [%test_result: int D.t] (D.bind t ~f) ~expect
;;

let%test_unit "uniform'" =
  let t = D.uniform' [ 0; 1; 2; 3 ] in
  let expect =
    [ ( 0, 0.25 )
    ; ( 1, 0.25 )
    ; ( 2, 0.25 )
    ; ( 3, 0.25 ) ]
    |> D.of_alist_exn
  in
  [%test_result: int D.t] t ~expect
;;

let%test_unit "cartesian_product" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map d1 = gen_distribution
    and     d2 = gen_distribution in
    (d1, d2)
  in
  Quickcheck.test gen
    ~sexp_of:[%sexp_of: int D.t * int D.t]
    ~f:(fun (d1, d2) ->
      let d = D.cartesian_product d1 d2 in
      Map.iteri (D.to_map d1) ~f:(fun ~key:k1 ~data:p1 ->
        Map.iteri (D.to_map d2) ~f:(fun ~key:k2 ~data:p2 ->
          [%test_result: float] ~expect:(p1 *. p2)
            (D.find_exn d (k1, k2)))))
;;
