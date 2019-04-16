open! Core
open! Import

module type S = Number_theory_intf.S

let[@inline never] raise_negative_exponent e =
  raise_s [%message "negative exponent" (e : int)]
;;

let[@inline never] raise_exponent_too_large e =
  raise_s [%message "large exponent not supported" (e : int)]
;;

(* https://en.wikipedia.org/wiki/Addition-chain_exponentiation

   Table at: http://wwwhomes.uni-bielefeld.de/achim/addition_chain.html *)
let[@inline always] addition_chain_pow_gen ~one ~mul:( * ) a exponent =
  match exponent with
  | 0 -> one
  | 1 -> a
  | 2 -> a * a
  | 3 -> a * a * a
  | 4 ->
    let b = a * a in
    b * b
  | 5 ->
    let b = a * a in
    b * b * a
  | 6 ->
    let b = a * a in
    b * b * b
  | 7 ->
    let b = a * a in
    b * b * b * a
  | 8 ->
    let b = a * a in
    let d = b * b in
    d * d
  | 9 ->
    let c = a * a * a in
    c * c * c
  | 10 ->
    let b = a * a in
    let d = b * b in
    d * d * b
  | 11 ->
    let b = a * a in
    let d = b * b in
    d * d * b * a
  | 12 ->
    let b = a * a in
    let d = b * b in
    d * d * d
  | 13 ->
    let b = a * a in
    let d = b * b in
    d * d * d * a
  | 14 ->
    let b = a * a in
    let d = b * b in
    d * d * d * b
  | 15 ->
    let b = a * a in
    let e = b * b * a in
    e * e * e
  | 16 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    h * h
  | 17 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    h * h * a
  | 18 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    h * h * b
  | 19 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    h * h * b * a
  | 20 ->
    let b = a * a in
    let e = b * b * a in
    let j = e * e in
    j * j
  | 21 ->
    let c = a * a * a in
    let f = c * c in
    f * f * f * c
  | 22 ->
    let b = a * a in
    let e = b * b * a in
    let k = e * e * a in
    k * k
  | 23 ->
    let b = a * a in
    let c = b * a in
    let e = c * b in
    let j = e * e in
    j * j * c
  | 24 ->
    let c = a * a * a in
    let f = c * c in
    let l = f * f in
    l * l
  | 25 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    h * h * h * a
  | 26 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    let m = h * d * a in
    m * m
  | 27 ->
    let c = a * a * a in
    let f = c * c in
    let l = f * f in
    l * l * c
  | 28 ->
    let b = a * a in
    let c = b * a in
    let e = c * b in
    let g = e * b in
    let n = g * g in
    n * n
  | 29 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    h * h * h * d * a
  | 30 ->
    let b = a * a in
    let e = b * b * a in
    let o = e * e * e in
    o * o
  | 31 ->
    let b = a * a in
    let c = b * a in
    let e = c * b in
    let g = e * b in
    let n = g * g in
    n * n * c
  | 32 ->
    let b = a * a in
    let d = b * b in
    let h = d * d in
    let p = h * h in
    p * p
  | e -> if e < 0 then raise_negative_exponent e else raise_exponent_too_large e
;;

module Make (Integer : Int_intf.S_unbounded) : S with type integer = Integer.t = struct
  open Integer.O

  type integer = Integer.t [@@deriving sexp]

  let one = Integer.one
  let two = Integer.of_int_exn 2
  let four = Integer.of_int_exn 4
  let six = Integer.of_int_exn 6
  let ten = Integer.of_int_exn 10

  let prime_cache =
    [ 2
    ; 3
    ; 5
    ; 7
    ; 11
    ; 13
    ; 17
    ; 19
    ; 23
    ; 29
    ; 31
    ; 37
    ; 41
    ; 43
    ; 47
    ; 53
    ; 59
    ; 61
    ; 67
    ; 71
    ; 73
    ; 79
    ; 83
    ; 89
    ; 97
    ]
    |> List.map ~f:Integer.of_int_exn
    |> Queue.of_list
  ;;

  let next_probable_prime n =
    match n % six |> Integer.to_int_exn with
    | 1 -> n + four
    | 5 -> n + two
    | 0 -> Integer.succ n
    | 2 -> Integer.succ n
    | 3 -> n + two
    | 4 -> Integer.succ n
    | _ -> assert false
  ;;

  let isqrt n =
    let rec loop n =
      if n < two
      then n
      else (
        let small = loop (n asr 2) lsl 1 in
        let large = small + one in
        if large * large > n then small else large)
    in
    if n < zero then raise_s [%message "isqrt" (n : integer)] else loop n
  ;;

  let is_perfect_square n =
    let sqrt = isqrt n in
    sqrt * sqrt = n
  ;;

  let rec is_prime n =
    if n <= one
    then false
    else (
      prepare_prime_cache ~upto:(one + isqrt n);
      with_return (fun { return } ->
        Queue.iter prime_cache ~f:(fun d ->
          if d * d > n then return true;
          if n % d = zero then return false);
        true))

  and extend_prime_cache () =
    let rec loop candidate =
      if is_prime candidate
      then Queue.enqueue prime_cache candidate
      else loop (next_probable_prime candidate)
    in
    loop (next_probable_prime (Queue.last_exn prime_cache))

  and prepare_prime_cache ~upto =
    while Queue.last_exn prime_cache < upto do
      extend_prime_cache ()
    done
  ;;

  let nth_prime i =
    let open Int.O in
    while i >= Queue.length prime_cache do
      extend_prime_cache ()
    done;
    Queue.get prime_cache i
  ;;

  let rec next_prime n =
    let next = next_probable_prime n in
    if is_prime next then next else next_prime next
  ;;

  let primes =
    Sequence.unfold ~init:two ~f:(fun p ->
      let next = next_prime p in
      Some (p, next))
  ;;

  let range ?(stride = one) ?(start = `inclusive) ?(stop = `exclusive) a b =
    let init =
      match start with
      | `inclusive -> a
      | `exclusive -> a + stride
    in
    let ( <= ) =
      match stop with
      | `inclusive -> ( <= )
      | `exclusive -> ( < )
    in
    let ( <= ) =
      match Integer.sign stride with
      | Neg -> fun left right -> right <= left
      | Pos -> fun left right -> left <= right
      | Zero -> invalid_arg "stride is zero"
    in
    Sequence.unfold_step ~init ~f:(fun n ->
      if n <= b then Yield (n, n + stride) else Done)
  ;;

  (* TODO handle negative numbers? *)
  let fold_digits ?(base = ten) n ~init ~f =
    let rec fold_digits_loop n ~base ~init ~f =
      if n = zero
      then init
      else fold_digits_loop ~base (n / base) ~init:(f init (n % base)) ~f
    in
    if n = zero then f init zero else fold_digits_loop ~base n ~init ~f
  ;;

  let iter_digits ?(base = ten) n ~f =
    let rec iter_digits_loop n ~base ~f =
      if n <> zero
      then (
        f (n % base);
        iter_digits_loop ~base (n / base) ~f)
    in
    if n = zero then f zero else iter_digits_loop ~base n ~f
  ;;

  let to_digits ?(base = ten) n = fold_digits ~base n ~init:[] ~f:(fun ac x -> x :: ac)

  let of_digits ?(base = ten) ds =
    Sequence.fold ds ~init:zero ~f:(fun acc n -> (base * acc) + n)
  ;;

  let sum_digits ?(base = ten) n =
    let rec iter n acc = if n = zero then acc else iter (n / base) ((n % base) + acc) in
    iter n zero
  ;;

  let rec gcd a b = if b = zero then a else gcd b (a % b)
  let lcm a b = a / gcd a b * b
  let factorial n = range two n ~stop:`inclusive |> Sequence.fold ~init:one ~f:( * )

  let prime_factor n =
    let rec loop_over_primes n i ac =
      let prime = nth_prime i in
      if prime * prime > n
      then if n > one then (n, 1) :: ac else ac
      else (
        let rec loop_over_powers n prime count ac =
          if n % prime = zero
          then loop_over_powers (n / prime) prime (succ count) ac
          else (
            let ac = if Int.(count > 0) then (prime, count) :: ac else ac in
            loop_over_primes n (succ i) ac)
        in
        loop_over_powers n prime 0 ac)
    in
    loop_over_primes n 0 [] |> List.rev
  ;;

  let factor n = prime_factor n |> Sequences.run_length_decode

  let divisors n =
    let mult_aux p a lst =
      let powers =
        Sequence.range ~stop:`inclusive 0 a
        |> Sequence.map ~f:Integer.of_int_exn
        |> Sequence.map ~f:(Integer.pow p)
        |> Sequence.to_list
      in
      List.map ~f:(fun pp -> List.map ~f:(fun x -> pp * x) lst) powers |> List.concat
    in
    let rec div_aux pfs lst =
      match pfs with
      | [] -> lst
      | (p, a) :: tl -> div_aux tl (mult_aux p a lst)
    in
    div_aux (prime_factor n) [ one ]
  ;;

  let num_divisors n =
    prime_factor n
    |> List.fold ~init:one ~f:(fun acc (_, a) -> acc * (Integer.of_int_exn a + one))
  ;;

  let totient n =
    prime_factor n
    |> List.fold ~init:one ~f:(fun acc (p, a) ->
      let pam1 = Integer.pow p (Integer.of_int_exn a - one) in
      acc * pam1 * (p - one))
  ;;

  let binomial n r =
    let top = range ~stop:`inclusive (r + one) n in
    let bottom = range ~stop:`inclusive one (n - r) in
    Sequence.zip top bottom |> Sequence.fold ~init:one ~f:(fun acc (t, b) -> acc * t / b)
  ;;

  let powmod a b ~modulus =
    if a < zero then raise_s [%message "powmod negative base" (a : integer)];
    let rec loop a b ac =
      if b = zero
      then ac
      else if b % two = zero
      then loop (a * a % modulus) (b / two) ac
      else loop a (b - one) (ac * a % modulus)
    in
    loop a b one
  ;;

  let bezout a b =
    let rec loop r0 s0 t0 r1 s1 t1 =
      if r1 = zero
      then s0, t0, r0
      else (
        let q = r0 / r1 in
        loop r1 s1 t1 (r0 - (q * r1)) (s0 - (q * s1)) (t0 - (q * t1)))
    in
    loop a one zero b zero one
  ;;

  let chinese_remainder_theorem residues =
    List.reduce_balanced_exn residues ~f:(fun (r1, m1) (r2, m2) ->
      let s, t, g = bezout m1 m2 in
      if g <> one
      then raise_s [%message "moduli not coprime" (m1 : integer) (m2 : integer)];
      let x = (r1 * t * m2) + (r2 * s * m1) in
      x, m1 * m2)
  ;;

  let fibonacci =
    Sequence.unfold ~init:(zero, one) ~f:(fun (a, b) -> Some (a, (b, a + b)))
  ;;

  (* https://www.nayuki.io/page/fast-fibonacci-algorithms *)
  let fast_fibonacci n =
    (* fib_n_n' n = (F(n), F(n + 1)) *)
    let rec fib_n_n' n =
      if n = zero
      then zero, one
      else (
        let a, b = fib_n_n' (n / two) in
        let c = a * ((b * two) - a) in
        let d = (a * a) + (b * b) in
        if n % two = zero then c, d else d, c + d)
    in
    fst (fib_n_n' n)
  ;;

  let natural_numbers ?(init = zero) () =
    Sequence.unfold ~init ~f:(fun n -> Some (n, n + one))
  ;;

  let addition_chain_pow = addition_chain_pow_gen ~one ~mul:( * )
end

(* Copied from above to avoid functorization cost. *)
let[@inline always] int_next_probable_prime n =
  match n mod 6 with
  | 1 -> n + 4
  | 5 -> n + 2
  | 0 -> n + 1
  | 2 -> n + 1
  | 3 -> n + 2
  | 4 -> n + 1
  | _ -> assert false
;;

let prime_sieve limit =
  let len = limit + 1 in
  let primes = Array.create ~len true in
  let rec mark p n =
    if n < len
    then (
      primes.(n) <- false;
      mark p (n + p))
  in
  let rec sieve p =
    if p * p < len
    then (
      if primes.(p) then mark p (p * p);
      sieve (int_next_probable_prime p))
  in
  primes.(0) <- false;
  primes.(1) <- false;
  sieve 2;
  primes
;;

let factorial_prime_factor n =
  let primes = prime_sieve n in
  let factors = ref [] in
  Array.iteri primes ~f:(fun p is_prime ->
    if is_prime
    then (
      let rec loop n ac = if n = 0 then ac else loop (n / p) ((n / p) + ac) in
      factors := (p, loop n 0) :: !factors));
  List.rev !factors
;;

(* TODO functorize *)
let multinomial xs =
  let p = ref 1 in
  let n = ref 1 in
  List.iter xs ~f:(fun x ->
    for i = 1 to x do
      p := !p * !n / i;
      incr n
    done);
  !p
;;

module Int = struct
  include Make (Int)

  (* shadow functorized isqrt implementation with fast but possibly raising version *)

  let isqrt_int_upper_bound =
    let mantissa_bits = 52 in
    1 lsl mantissa_bits
  ;;

  let[@inline never] raise_isqrt_overflow n =
    raise_s [%message "isqrt would lose precision due to overflow" (n : int)]
  ;;

  let[@inline always] isqrt n =
    if n < isqrt_int_upper_bound
    then Float.iround_down_exn (sqrt (float n))
    else raise_isqrt_overflow n
  ;;

  let[@inline always] is_perfect_square n =
    let s = isqrt n in
    s * s = n
  ;;

  (* TODO Maybe functorize when flambda becomes the default. *)
  let[@inline always] addition_chain_pow b e =
    addition_chain_pow_gen b e ~one:1 ~mul:( * )
  ;;
end

module Bigint = struct
  include Make (struct
      include Bigint

      (* different signature in Int_intf.S_unbounded *)
      let to_int64 _t = raise_s [%message "unimplemented" "to_int64"]
    end)

  let[@inline always] addition_chain_pow b e =
    addition_chain_pow_gen b e ~one:Bigint.one ~mul:Bigint.( * )
  ;;
end

let%test_unit "addition_chain_pow vs. Int.pow" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%map () = return ()
    and b = Core.Int.gen_incl 0 3
    and e = Core.Int.gen_incl 0 32 in
    b, e
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: int * int] ~f:(fun (b, e) ->
    [%test_result: int] (Int.addition_chain_pow b e) ~expect:(Core.Int.pow b e))
;;
