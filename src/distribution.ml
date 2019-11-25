open! Core
open! Import
include Distribution_intf

module Make (Prob : Prob) : S with module Prob = Prob = struct
  module Prob = Prob

  type ('key, 'cmp) key = ('key, 'cmp) Map.comparator
  type ('key, 'cmp) t = ('key, Prob.t, 'cmp) Map.t

  let compare a b = Map.compare_direct Prob.compare a b

  let sexp_of_t (type key) sexp_of_key =
    Map.sexp_of_m__t
      (module struct
        type t = key [@@deriving sexp_of]
      end)
      Prob.sexp_of_t
  ;;

  let quickcheck_generator (type k c) ((module Key) : (k, c) key) quickcheck_generator_key
    =
    Map.quickcheck_generator
      (module Key)
      quickcheck_generator_key
      Prob.quickcheck_generator
  ;;

  let quickcheck_observer quickcheck_observer_key =
    Map.quickcheck_observer quickcheck_observer_key Prob.quickcheck_observer
  ;;

  let quickcheck_shrinker quickcheck_shrinker_key =
    Map.quickcheck_shrinker quickcheck_shrinker_key Prob.quickcheck_shrinker
  ;;

  let one = Prob.of_int 1
  let support = Map.keys
  let singleton key x = Map.singleton key x one
  let scale t x = Map.map t ~f:(fun p -> Prob.(p * x))
  let total t = Map.data t |> List.sum (module Prob) ~f:Fn.id
  let normalize t = scale t Prob.(one / total t)

  let merge t1 t2 =
    Map.merge t1 t2 ~f:(fun ~key:_ ->
      function
      | `Both (p1, p2) -> Some Prob.(p1 + p2)
      | `Left p1 -> Some p1
      | `Right p2 -> Some p2)
  ;;

  let combine ~d1 ~d2 ~p1 = merge (scale d1 p1) (scale d2 Prob.(one - p1))

  let uniform ts =
    match List.length ts with
    | 0 -> invalid_arg "uniform"
    | n ->
      let x = Prob.(of_int 1 / of_int n) in
      ts |> List.map ~f:(fun t -> scale t x) |> List.reduce_exn ~f:merge
  ;;

  let uniform' key xs = uniform (List.map xs ~f:(singleton key))
  let find = Map.find
  let find_or_zero t x = Map.find t x |> Option.value ~default:Prob.zero
  let find_exn = Map.find_exn
  let of_map = Fn.id
  let to_map = Fn.id
  let of_alist = Map.of_alist_reduce ~f:Prob.( + )
  let of_alist_exn = Map.of_alist_exn
  let to_alist = Map.to_alist
  let return = singleton

  let bind' key t ~f =
    Map.fold t ~init:(Map.empty key) ~f:(fun ~key ~data:p1 ac ->
      f key
      |> Map.fold ~init:ac ~f:(fun ~key ~data:p2 ac ->
        Map.update ac key ~f:(function
          | None -> Prob.(p1 * p2)
          | Some p -> Prob.(p + (p1 * p2)))))
  ;;

  let bind t ~f = bind' (Map.comparator_s t) t ~f

  let map' key t ~f =
    Map.fold t ~init:(Map.empty key) ~f:(fun ~key ~data ac ->
      Map.update ac (f key) ~f:(function
        | None -> data
        | Some p -> Prob.(p + data)))
  ;;

  let map t ~f = map' (Map.comparator_s t) t ~f

  let combine' (type k1 k2 c1 c2) ~d1 ~d2 ~p1 =
    let sexp_of_k1 = (Map.comparator d1).sexp_of_t in
    let sexp_of_k2 = (Map.comparator d2).sexp_of_t in
    let key : ((k1, k2) Either.t, (c1, c2) Either.comparator_witness) key =
      (module struct
        type t = (k1, k2) Either.t [@@deriving sexp_of]
        type comparator_witness = (c1, c2) Either.comparator_witness

        let comparator = Either.comparator (Map.comparator d1) (Map.comparator d2)
      end)
    in
    match
      Map.append
        ~lower_part:(scale (map' key d1 ~f:Either.first) p1)
        ~upper_part:(scale (map' key d2 ~f:Either.second) Prob.(one - p1))
    with
    | `Ok map -> map
    | `Overlapping_key_ranges ->
      (* [First] compares smaller than [Second], so this is safe. *)
      raise_s
        [%message
          "Euler.Distribution.combine': Expected First to compare smaller than Second"]
  ;;

  let cartesian_product (type k1 k2 c1 c2) t1 t2 =
    let sexp_of_k1 = (Map.comparator t1).sexp_of_t in
    let sexp_of_k2 = (Map.comparator t2).sexp_of_t in
    let key : (k1 * k2, (c1, c2) Tuple2.comparator_witness) key =
      (module struct
        type t = k1 * k2 [@@deriving sexp_of]
        type comparator_witness = (c1, c2) Tuple2.comparator_witness

        let comparator = Tuple2.comparator (Map.comparator t1) (Map.comparator t2)
      end)
    in
    t1 |> bind' key ~f:(fun k1 -> t2 |> map' key ~f:(fun k2 -> k1, k2))
  ;;

  let both = cartesian_product

  (* TODO: This could be implemented more efficiently. *)
  let map2 t1 t2 ~f = bind t1 ~f:(fun a -> map t2 ~f:(fun b -> f a b))

  module Infix = struct
    let ( >>| ) t f = map t ~f
    let ( >>= ) t f = bind t ~f
  end

  include Infix

  module Let_syntax = struct
    include Infix

    let return = return

    module Let_syntax = struct
      let map = map
      let bind = bind
      let both = both

      (* TODO: Add some useful functions here. *)
      module Open_on_rhs = struct end
    end
  end

  module M (K : sig
      type t
      type comparator_witness
    end) =
  struct
    type nonrec t = (K.t, Prob.t, K.comparator_witness) Map.t
  end

  let compare_m__t key a b = Map.compare_m__t key Prob.compare a b
  let sexp_of_m__t key t = Map.sexp_of_m__t key [%sexp_of: Prob.t] t

  let quickcheck_generator_m__t (type k c) key =
    let (module Key : Quickcheck_m with type t = k and type comparator_witness = c) =
      key
    in
    quickcheck_generator (module Key) Key.quickcheck_generator
  ;;

  let quickcheck_observer_m__t (type k c) key =
    let (module Key : Quickcheck_m with type t = k and type comparator_witness = c) =
      key
    in
    quickcheck_observer Key.quickcheck_observer
  ;;

  let quickcheck_shrinker_m__t (type k c) key =
    let (module Key : Quickcheck_m with type t = k and type comparator_witness = c) =
      key
    in
    quickcheck_shrinker Key.quickcheck_shrinker
  ;;
end

module Float = Make (struct
    include Float

    let quickcheck_generator = Float.gen_incl 0. 1.
    let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
  end)

module Percent = Make (struct
    include Percent

    let of_int n = of_mult (float n)
    let ( / ) x y = of_mult (to_mult x /. to_mult y)

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
