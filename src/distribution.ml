open! Core
open! Import

module type Prob = Distribution_intf.Prob
module type S = Distribution_intf.S

module Make (Prob : Prob) : S with module Prob = Prob = struct
  module Prob = Prob

  (* FIXME See if you can avoid using [Map.Poly.t]. *)
  type 'key t = ('key, Prob.t) Map.Poly.t [@@deriving compare, quickcheck, sexp]

  let one = Prob.of_int 1
  let support = Map.keys
  let singleton key = Map.Poly.singleton key one
  let scale t x = Map.map t ~f:(fun p -> Prob.(p * x))
  let total t = Map.data t |> List.sum (module Prob) ~f:Fn.id
  let normalize t = scale t Prob.(one / total t)

  let merge t1 t2 =
    Map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Both (p1, p2) -> Some Prob.(p1 + p2)
      | `Left p1 -> Some p1
      | `Right p2 -> Some p2)
  ;;

  let combine ~d1 ~d2 ~p1 = merge (scale d1 p1) (scale d2 Prob.(one - p1))

  let uniform ts =
    match List.length ts with
    | 0 -> invalid_arg "uniform"
    | n ->
      let x = Prob.(1 // n) in
      ts |> List.map ~f:(fun t -> scale t x) |> List.reduce_exn ~f:merge
  ;;

  let uniform' ks = uniform (List.map ks ~f:singleton)
  let find = Map.find
  let find_or_zero t x = Map.find t x |> Option.value ~default:Prob.zero
  let find_exn = Map.find_exn
  let of_map = Fn.id
  let to_map = Fn.id
  let of_alist_exn = Map.Poly.of_alist_exn
  let to_alist t = Map.to_alist t

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let return = singleton

      let bind t ~f =
        Map.fold t ~init:Map.Poly.empty ~f:(fun ~key ~data:p1 ac ->
          f key
          |> Map.fold ~init:ac ~f:(fun ~key ~data:p2 ac ->
            Map.update ac key ~f:(function
              | None -> Prob.(p1 * p2)
              | Some p -> Prob.(p + (p1 * p2)))))
      ;;

      let map t ~f =
        Map.fold t ~init:Map.Poly.empty ~f:(fun ~key ~data ac ->
          Map.update ac (f key) ~f:(function
            | None -> data
            | Some p -> Prob.(p + data)))
      ;;

      let map = `Custom map
    end)

  let combine' ~d1 ~d2 ~p1 =
    merge
      (scale (map d1 ~f:Either.first) p1)
      (scale (map d2 ~f:Either.second) Prob.(one - p1))
  ;;

  let cartesian_product t1 t2 =
    let open Let_syntax in
    let%bind k1 = t1 in
    let%map k2 = t2 in
    k1, k2
  ;;
end

module Float = Make (struct
    include Float

    let ( // ) x y = of_int x / of_int y
    let quickcheck_generator = Float.gen_incl 0. 1.
    let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
  end)

module Percent = Make (struct
    include Percent

    let of_int n = of_mult (float n)
    let ( / ) x y = of_mult (to_mult x /. to_mult y)
    let ( // ) x y = of_mult (float x /. float y)

    let quickcheck_generator =
      Float.Prob.quickcheck_generator |> Quickcheck.Generator.map ~f:of_mult
    ;;

    let quickcheck_shrinker =
      Float.Prob.quickcheck_shrinker
      |> Quickcheck.Shrinker.map ~f:of_mult ~f_inverse:to_mult
    ;;

    let quickcheck_observer =
      Float.Prob.quickcheck_observer |> Quickcheck.Observer.unmap ~f:to_mult
    ;;
  end)

module Bignum = Make (struct
    include Bignum

    let quickcheck_generator = Bignum.gen_incl Bignum.zero Bignum.one
    let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
  end)
