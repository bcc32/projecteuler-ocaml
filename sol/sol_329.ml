open! Core
open! Import

module M = struct
  let problem = `Number 329

  module Dist = Distribution.Make(Bignum)

  let is_prime = lazy (Number_theory.prime_sieve 500)

  let step (pos : int Dist.t) : (int * [ `Prime | `Not_prime ]) Dist.t =
    let open Dist.Let_syntax in
    let%bind x = pos in
    let sound =
      if (force is_prime).(x)
      then (Dist.uniform' [ `Prime; `Prime; `Not_prime ])
      else (Dist.uniform' [ `Prime; `Not_prime; `Not_prime ])
    in
    let pos =
      match x with
      | 1   -> return 2
      | 500 -> return 499
      | x   -> Dist.uniform' [ (x - 1); (x + 1) ]
    in
    Dist.cartesian_product pos sound
  ;;

  let main () =
    let init = ref (Dist.uniform' (List.range 1 500 ~stop:`inclusive)) in
    let product = ref Bignum.one in
    let expected =
      "PPPPNNPPPNPPNPN"
      |> String.to_list
      |> List.map ~f:(function
        | 'P' -> `Prime
        | 'N' -> `Not_prime
        | _ -> assert false)
    in
    List.iter expected ~f:(fun expected ->
      assert (Map.length (Dist.to_map !init) = 500);
      let next =
        step !init
        |> Dist.to_alist
        |> List.filter_map ~f:(fun ((pos, is_prime), p) ->
          if is_prime = expected
          then Some (pos, p)
          else None)
        |> Dist.of_alist_exn
      in
      let total = Dist.total next in
      product := Bignum.(!product * total);
      let next = Dist.normalize next in
      init := next);
    printf !"%{Bigint}/%{Bigint}\n"
      (Bignum.num_as_bigint !product)
      (Bignum.den_as_bigint !product)
  ;;
  (* 199740353/29386561536000
     80ms *)
end

include Solution.Make(M)
