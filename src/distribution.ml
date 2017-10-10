open! Core

module Make (Prob : Distribution_intf.Prob) = struct
  (* FIXME See if you can avoid using [Map.Poly.t]. *)
  type 'key t = ('key, Prob.t) Map.Poly.t [@@deriving compare, sexp]

  type prob = Prob.t

  let one = Prob.of_int 1

  let singleton key = Map.Poly.singleton key one

  let scale t x = Map.map t ~f:(fun p -> Prob.(p * x))

  let combine ~d1 ~d2 ~p1 =
    let p2 = Prob.(one - p1) in
    Map.merge (scale d1 p1) (scale d2 p2) ~f:(fun ~key:_ -> function
      | `Both (p1, p2) -> Some Prob.(p1 + p2)
      | `Left p1       -> Some p1
      | `Right p2      -> Some p2)
  ;;

  let find     = Map.find
  let find_exn = Map.find_exn

  let of_map = Fn.id
  let to_map = Fn.id

  include Monad.Make(struct
      type nonrec 'a t = 'a t

      let return = singleton

      let bind t ~f =
        Map.fold t ~init:Map.Poly.empty ~f:(fun ~key ~data:p1 ac ->
          f key
          |> Map.fold ~init:ac ~f:(fun ~key ~data:p2 ac ->
            Map.update ac key ~f:(function
              | None   -> Prob.(p1 * p2)
              | Some p -> Prob.(p + p1 * p2))))
      ;;

      let map t ~f =
        Map.fold t ~init:Map.Poly.empty ~f:(fun ~key ~data ac ->
          Map.update ac (f key) ~f:(function
            | None   -> data
            | Some p -> Prob.(p + data)))
      ;;

      let map = `Custom map
    end)
end
