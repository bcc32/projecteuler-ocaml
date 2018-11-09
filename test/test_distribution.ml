open! Core
open! Import
module D = Distribution.Bignum

let gen_prob = Bignum.(gen_incl zero one)

let gen_distribution =
  let open Gen.Let_syntax in
  let%map keys = Int.Map.gen Int.gen gen_prob in
  keys |> Map.to_alist |> D.of_alist_exn |> D.normalize
;;

let%test_unit "singleton" =
  let d = D.singleton 0 in
  [%test_result: Bignum.t] ~expect:Bignum.one (D.find_exn d 0)
;;

let%test_unit "scale" =
  Q.test gen_prob ~f:(fun p ->
    let d = D.scale (D.singleton 0) p in
    [%test_result: Bignum.t] ~expect:p (D.find_exn d 0))
;;

let%test_unit "combine" =
  let d1 = D.singleton 1 in
  let d2 = D.singleton 2 in
  Q.test gen_prob ~f:(fun p1 ->
    let d = D.combine ~d1 ~d2 ~p1 in
    [%test_result: Bignum.t] (D.find_exn d 1) ~expect:p1;
    [%test_result: Bignum.t] (D.find_exn d 2) ~expect:Bignum.(one - p1))
;;

let%test_unit "monad laws" =
  let f x = D.singleton (x + 1) in
  let g x =
    let p1 = Bignum.(one / of_int 2) in
    D.combine ~p1 ~d1:(D.singleton x) ~d2:(D.singleton (-x))
  in
  let t = D.singleton 0 in
  Q.test Int.gen ~f:(fun v ->
    let open D.Let_syntax in
    [%test_result: int D.t] (return v >>= f) ~expect:(f v);
    [%test_result: int D.t] (return v >>= g) ~expect:(g v);
    [%test_result: int D.t] (t >>= return) ~expect:t;
    [%test_result: int D.t] (t >>= f >>= g) ~expect:(t >>= fun x -> f x >>= g);
    [%test_result: int D.t] (t >>= g >>= f) ~expect:(t >>= fun x -> g x >>= f))
;;

let%test_unit "bind" =
  let half = Bignum.(one / of_int 2) in
  let fifth = Bignum.(one / of_int 5) in
  let f x = D.combine ~p1:half ~d1:(D.singleton x) ~d2:(D.singleton (x + 1)) in
  let t = D.combine ~p1:fifth ~d1:(D.singleton 0) ~d2:(D.singleton 1) in
  let expect =
    [ (0, Bignum.(of_int 1 / ten))
    ; (1, Bignum.(of_int 5 / ten))
    ; (2, Bignum.(of_int 4 / ten))
    ]
    |> D.of_alist_exn
  in
  [%test_result: int D.t] (D.bind t ~f) ~expect
;;

let%test_unit "uniform" =
  List.gen_non_empty gen_distribution
  |> Q.test
       ~sexp_of:[%sexp_of: int D.t list]
       ~shrinker:(List.shrinker (Shr.empty ()))
       ~f:(fun ds ->
         let n = List.length ds in
         let d = D.uniform ds in
         Map.iteri (D.to_map d) ~f:(fun ~key ~data ->
           let expect =
             let open Bignum.O in
             List.sum
               (module Bignum)
               ds
               ~f:(fun d -> D.find d key |> Option.value ~default:Bignum.zero)
             / of_int n
           in
           [%test_result: Bignum.t] data ~expect))
;;

let%test_unit "uniform'" =
  let gen =
    let open Gen.Let_syntax in
    let%map xs = List.gen_non_empty Int.gen in
    List.dedup_and_sort xs ~compare:Int.compare
  in
  Q.test
    gen
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:(List.shrinker Int.shrinker)
    ~f:(fun ks ->
      let t = D.uniform' ks in
      let expect =
        let length = List.length ks in
        let prob = Bignum.(one / of_int length) in
        ks |> List.map ~f:(fun k -> k, prob) |> D.of_alist_exn
      in
      [%test_result: int D.t] t ~expect)
;;

let%test_unit "cartesian_product" =
  let gen = Gen.both gen_distribution gen_distribution in
  Q.test gen ~sexp_of:[%sexp_of: int D.t * int D.t] ~f:(fun (d1, d2) ->
    let d = D.cartesian_product d1 d2 in
    Map.iteri (D.to_map d1) ~f:(fun ~key:k1 ~data:p1 ->
      Map.iteri (D.to_map d2) ~f:(fun ~key:k2 ~data:p2 ->
        [%test_result: Bignum.t] ~expect:Bignum.(p1 * p2) (D.find_exn d (k1, k2)))))
;;
