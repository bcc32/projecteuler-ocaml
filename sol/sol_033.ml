open! Core
open! Import

module M = struct
  let problem = Number 33

  let main () =
    let frac n d = Bignum.(of_int n / of_int d) in
    let fractions =
      let open Sequence.Let_syntax in
      let%bind n1 = Sequence.range 1 10 in
      let%bind n2 = Sequence.range 0 10 in
      let%bind d1 = Sequence.range 1 10 in
      let%bind d2 = Sequence.range 0 10 in
      let n = (10 * n1) + n2 in
      let d = (10 * d1) + d2 in
      if n >= d || (n2 = 0 && d2 = 0)
      then Sequence.empty
      else if n1 = d2 && Bignum.( = ) (frac n2 d1) (frac n d)
      then (
        (* this case isn't used *)
        if debug then Debug.eprintf "case 1: %d/%d = %d/%d" n d n2 d1;
        return (frac n d))
      else if n2 = d1 && Bignum.( = ) (frac n1 d2) (frac n d)
      then (
        if debug then Debug.eprintf "case 2: %d/%d = %d/%d" n d n1 d2;
        return (frac n d))
      else Sequence.empty
    in
    fractions
    |> Sequence.fold ~init:Bignum.one ~f:Bignum.( * )
    |> Bignum.den_as_bigint
    |> printf !"%{Bigint}\n"
  ;;

  (* 100
     0.926ms *)
end

include Solution.Make (M)