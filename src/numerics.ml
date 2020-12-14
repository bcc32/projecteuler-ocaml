open! Core
open! Import
include Numerics_intf

module Make (Real : Real) : S with type real = Real.t = struct
  open Real

  type real = Real.t

  let zero = of_int 0
  let two = of_int 2
  let four = of_int 4
  let six = of_int 6

  let[@cold] raise_expected_positive_epsilon epsilon =
    raise_s [%message "expected positive epsilon" (epsilon : Real.t)]
  ;;

  let[@inline always] check_epsilon_is_positive epsilon =
    match sign_exn epsilon with
    | Neg | Zero -> raise_expected_positive_epsilon epsilon
    | Pos -> ()
  ;;

  let bisect ~f ~epsilon ~lo:x_lo ~hi:x_hi =
    check_epsilon_is_positive epsilon;
    let rec loop x_lo x_hi y_lo y_hi =
      let x_mi = (x_lo + x_hi) / two in
      if x_hi - x_lo < epsilon
      then x_mi
      else (
        let y_mi = f x_mi in
        match sign_exn y_mi with
        | Zero -> x_mi
        | Neg ->
          (match sign_exn y_lo with
           | Neg -> loop x_mi x_hi y_mi y_hi
           | Pos -> loop x_lo x_mi y_lo y_mi
           | Zero -> failwith "BUG: zero y-value endpoint")
        | Pos ->
          (match sign_exn y_lo with
           | Neg -> loop x_lo x_mi y_lo y_mi
           | Pos -> loop x_mi x_hi y_mi y_hi
           | Zero -> failwith "BUG: zero y-value endpoint"))
    in
    let y_lo = f x_lo in
    let y_hi = f x_hi in
    if y_lo = zero then x_lo else if y_hi = zero then x_hi else loop x_lo x_hi y_lo y_hi
  ;;

  let rec newton's_method ~f ~f' ~epsilon ~init =
    check_epsilon_is_positive epsilon;
    let delta = f init / f' init in
    if abs delta < epsilon
    then init
    else newton's_method ~f ~f' ~epsilon ~init:(init - delta)
  ;;

  let[@cold] raise_expected_lo_hi ~lo ~hi =
    raise_s [%message "expected lo < hi" (lo : Real.t) (hi : Real.t)]
  ;;

  let[@inline always] check_lo_hi ~lo ~hi =
    if not (lo < hi) then raise_expected_lo_hi ~lo ~hi
  ;;

  let[@cold] raise_expected_positive_intervals intervals =
    raise_s [%message "expected intervals > 0" (intervals : int)]
  ;;

  let integrate ?(method_ = `Simpson's_rule) () ~f ~lo ~hi ~intervals =
    let rule =
      match method_ with
      | `Midpoint -> fun x_lo _ x_hi _ -> f ((x_lo + x_hi) / two)
      | `Trapezoid -> fun _ y_lo _ y_hi -> (y_lo + y_hi) / two
      | `Simpson's_rule ->
        fun x_lo y_lo x_hi y_hi -> ((four * f ((x_lo + x_hi) / two)) + y_lo + y_hi) / six
    in
    if Int.( <= ) intervals 0 then raise_expected_positive_intervals intervals;
    check_lo_hi ~lo ~hi;
    let dx = (hi - lo) / of_int intervals in
    let rec loop x_lo y_lo i ac =
      if Int.(i >= intervals)
      then ac
      else (
        let x_hi = x_lo + dx in
        let y_hi = f x_hi in
        loop x_hi y_hi Int.(i + 1) (ac + (dx * rule x_lo y_lo x_hi y_hi)))
    in
    loop lo (f lo) 0 zero
  ;;
end

module Float = Make (Float)

module Bignum = Make (struct
    include Bignum

    let sign_exn t = Sign.of_int (sign t)
  end)
