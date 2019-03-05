open! Core
open! Import

let gen_prob = Bignum.(gen_incl zero one)

module D = struct
  include Distribution.Bignum

  (* CR azeng: Should take key generator? *)
  let quickcheck_generator =
    let open Gen.Let_syntax in
    let%map keys = Int.Map.quickcheck_generator Int.quickcheck_generator gen_prob in
    keys |> Map.to_alist |> of_alist_exn |> normalize
  ;;
end

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
    let p1 = Bignum.(1 // 2) in
    D.combine ~p1 ~d1:(D.singleton x) ~d2:(D.singleton (-x))
  in
  let t = D.singleton 0 in
  Q.test Int.quickcheck_generator ~f:(fun v ->
    let open D.Let_syntax in
    [%test_result: int D.t] (return v >>= f) ~expect:(f v);
    [%test_result: int D.t] (return v >>= g) ~expect:(g v);
    [%test_result: int D.t] (t >>= return) ~expect:t;
    [%test_result: int D.t] (t >>= f >>= g) ~expect:(t >>= fun x -> f x >>= g);
    [%test_result: int D.t] (t >>= g >>= f) ~expect:(t >>= fun x -> g x >>= f))
;;

let%test_unit "bind" =
  let half = Bignum.(1 // 2) in
  let fifth = Bignum.(1 // 5) in
  let f x = D.combine ~p1:half ~d1:(D.singleton x) ~d2:(D.singleton (x + 1)) in
  let t = D.combine ~p1:fifth ~d1:(D.singleton 0) ~d2:(D.singleton 1) in
  let expect =
    [ (0, Bignum.(1 // 10)); (1, Bignum.(5 // 10)); (2, Bignum.(4 // 10)) ]
    |> D.of_alist_exn
  in
  [%test_result: int D.t] (D.bind t ~f) ~expect
;;

let%test_unit "uniform" =
  Q.test
    (List.gen_non_empty [%quickcheck.generator: D.t])
    ~sexp_of:[%sexp_of: int D.t list]
    ~shrinker:[%quickcheck.shrinker: _ list]
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
    let%map xs = List.gen_non_empty Int.quickcheck_generator in
    List.dedup_and_sort xs ~compare:Int.compare
  in
  Q.test
    gen
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:[%quickcheck.shrinker: int list]
    ~f:(fun ks ->
      let t = D.uniform' ks in
      let expect =
        let length = List.length ks in
        let prob = Bignum.(1 // length) in
        ks |> List.map ~f:(fun k -> k, prob) |> D.of_alist_exn
      in
      [%test_result: int D.t] t ~expect)
;;

let%test_unit "cartesian_product" =
  let gen = [%quickcheck.generator: D.t * D.t] in
  Q.test gen ~sexp_of:[%sexp_of: int D.t * int D.t] ~f:(fun (d1, d2) ->
    let d = D.cartesian_product d1 d2 in
    Map.iteri (D.to_map d1) ~f:(fun ~key:k1 ~data:p1 ->
      Map.iteri (D.to_map d2) ~f:(fun ~key:k2 ~data:p2 ->
        [%test_result: Bignum.t] ~expect:Bignum.(p1 * p2) (D.find_exn d (k1, k2)))
    ))
;;
