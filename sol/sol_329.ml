open! Core
open! Import

module Is_prime = struct
  type t =
    [ `Prime
    | `Not_prime
    ]
  [@@deriving compare, equal, quickcheck, sexp_of]

  let of_char_exn = function
    | 'P' -> `Prime
    | 'N' -> `Not_prime
    | c -> invalid_argf "Is_prime.of_char_exn: '%c'" c ()
  ;;

  include (val Comparator.make ~compare ~sexp_of_t)
end

module Dist = Distribution.Bignum

let is_prime = lazy (Number_theory.prime_sieve 500)

let step (pos : (int, _) Dist.t) : (int * Is_prime.t, _) Dist.t =
  Dist.bind'
    (module struct
      type t = int * Is_prime.t [@@deriving compare, quickcheck, sexp_of]

      type comparator_witness =
        (Int.comparator_witness, Is_prime.comparator_witness) Tuple.T2.comparator_witness

      let comparator = Tuple.T2.comparator Int.comparator Is_prime.comparator
    end)
    pos
    ~f:(fun x ->
      let sound =
        if (force is_prime).(x)
        then Dist.uniform' (module Is_prime) [ `Prime; `Prime; `Not_prime ]
        else Dist.uniform' (module Is_prime) [ `Prime; `Not_prime; `Not_prime ]
      in
      let pos =
        match x with
        | 1 -> Dist.singleton (module Int) 2
        | 500 -> Dist.singleton (module Int) 499
        | x -> Dist.uniform' (module Int) [ x - 1; x + 1 ]
      in
      Dist.cartesian_product pos sound)
;;

let main () =
  let init = ref (Dist.uniform' (module Int) (List.range 1 500 ~stop:`inclusive)) in
  let product = ref Bignum.one in
  let expected =
    "PPPPNNPPPNPPNPN" |> String.to_list |> List.map ~f:Is_prime.of_char_exn
  in
  List.iter expected ~f:(fun expected ->
    assert (Map.length (Dist.to_map !init) = 500);
    let next =
      step !init
      |> Dist.to_alist
      |> List.filter_map ~f:(fun ((pos, is_prime), p) ->
        if Is_prime.equal is_prime expected then Some (pos, p) else None)
      |> Dist.of_alist_exn (module Int)
    in
    let total = Dist.total next in
    (product := Bignum.(!product * total));
    let next = Dist.normalize next in
    init := next);
  printf
    !"%{Bigint}/%{Bigint}\n"
    (Bignum.num_as_bigint !product)
    (Bignum.den_as_bigint !product)
;;

(* 80ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 199740353/29386561536000 |}]
;;

include (val Solution.make ~problem:(Number 329) ~main)
