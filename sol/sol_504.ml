open! Core
open! Import

module M = struct
  let problem = Number 504
  let limit = 100

  (* https://en.wikipedia.org/wiki/Pick%27s_theorem *)

  let lattice_points x y =
    let boundary_points = x + y + Number_theory.Int.gcd x y in
    (((x * y) - boundary_points) / 2) + 1
  ;;

  let lattice_points =
    let cache =
      Array.init (limit + 1) ~f:(fun x ->
        Array.init (limit + 1) ~f:(fun y -> lattice_points x y))
    in
    fun x y -> cache.(x).(y)
  ;;

  let main () =
    let count = ref 0 in
    for a = 1 to limit do
      for b = 1 to limit do
        for c = 1 to limit do
          for d = 1 to limit do
            let points =
              (* (n - 1) points strictly inside on each axis, plus origin *)
              a
              + b
              + c
              + d
              - 3
              + lattice_points a b
              + lattice_points b c
              + lattice_points c d
              + lattice_points d a
            in
            if Number_theory.Int.is_perfect_square points then incr count
          done
        done
      done
    done;
    printf "%d\n" !count
  ;;

  (* 694687
     15s *)
end

include Solution.Make (M)
