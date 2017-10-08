open! Core

module Make (Real : Numerics_intf.Real) = struct
  open Real

  type real = Real.t

  let bisect =
    let two = of_int 2 in
    fun ~f ~epsilon ~low:x_lo ~high:x_hi ->
      let rec loop x_lo x_hi y_lo y_hi =
        let x_mi = (x_lo + x_hi) / two in
        if x_hi - x_lo < epsilon
        then x_mi
        else (
          let y_mi = f x_mi in
          match sign y_mi with
          | Zero -> x_mi
          | Neg ->
            begin match sign y_lo with
            | Neg -> loop x_mi x_hi y_mi y_hi
            | Pos -> loop x_lo x_mi y_lo y_mi
            | Zero -> raise (Bug "zero y-value endpoint")
            end
          | Pos ->
            begin match sign y_lo with
            | Neg -> loop x_lo x_mi y_lo y_mi
            | Pos -> loop x_mi x_hi y_mi y_hi
            | Zero -> raise (Bug "zero y-value endpoint")
            end)
      in
      let y_lo = f x_lo in
      let y_hi = f x_hi in
      loop x_lo x_hi y_lo y_hi
  ;;

  let rec newton's_method ~f ~f' ~epsilon ~init =
    let delta = f init / f' init in
    if abs delta < epsilon
    then init
    else newton's_method ~f ~f' ~epsilon ~init:(init - delta)
  ;;
end

module Float  = Make (Float)
module Bignum = Make (struct
    open Bignum.Std
    include Bignum
    let sign t = Sign.of_int (sign t)
  end)
