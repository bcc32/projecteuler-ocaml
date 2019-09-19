open! Core
open! Import

let%bench_module "Newton's method" =
  (module struct
    let sqrt_newton x =
      Numerics.Float.newton's_method
        ~f:(fun y -> Float.((y * y) - x))
        ~f':(Float.( * ) 2.0)
        ~epsilon:1e-12
        ~init:1.0
    ;;

    let%bench "1.0" = sqrt_newton 1.0
    let%bench "1.5" = sqrt_newton 1.5
    let%bench "2.0" = sqrt_newton 2.0
    let%bench "3.0" = sqrt_newton 3.0
    let%bench "100.0" = sqrt_newton 100.0
    let%bench "20_000.0" = sqrt_newton 20_000.0
  end)
;;

let%bench_module "bisection" =
  (module struct
    let sqrt_bisect x =
      Numerics.Float.bisect
        ~f:(fun y -> Float.((y * y) - x))
        ~epsilon:1e-12
        ~lo:1.0
        ~hi:x
    ;;

    let%bench "1.0" = sqrt_bisect 1.0
    let%bench "1.5" = sqrt_bisect 1.5
    let%bench "2.0" = sqrt_bisect 2.0
    let%bench "3.0" = sqrt_bisect 3.0
    let%bench "100.0" = sqrt_bisect 100.0
    let%bench "20_000.0" = sqrt_bisect 20_000.0
  end)
;;

let%bench_module "built-in [sqrt] function" =
  (module struct
    let%bench "1.0" = sqrt 1.0
    let%bench "1.5" = sqrt 1.5
    let%bench "2.0" = sqrt 2.0
    let%bench "3.0" = sqrt 3.0
    let%bench "100.0" = sqrt 100.0
    let%bench "20_000.0" = sqrt 20_000.0
  end)
;;
