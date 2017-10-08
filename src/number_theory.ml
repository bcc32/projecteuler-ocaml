open! Core

module Make (Int : Int_intf.S_unbounded) = struct
  open Int.O

  type integer = Int.t [@@deriving sexp]

  let one  = Int.one
  let two  = of_int_exn 2
  let four = of_int_exn 4

  let range ?(stride = one) ?(start = `inclusive) ?(stop = `exclusive) a b =
    let init =
      match start with
      | `inclusive -> a
      | `exclusive -> a + stride
    in
    let (<=) =
      match stop with
      | `inclusive -> (<=)
      | `exclusive -> (<)
    in
    let (<=) left right =
      match Int.sign stride with
      | Neg -> right <= left
      | Pos -> left  <= right
      | Zero -> invalid_arg "stride is zero"
    in
    Sequence.unfold ~init ~f:(fun n ->
      if n <= b
      then Some (n, n + stride)
      else None)

  let digits_of_int ?(base = of_int_exn 10) n =
    let rec aux n d =
      if n = zero
      then d
      else aux (n / base) (n % base :: d)
    in
    aux n []

  let int_of_digits ?(base = of_int_exn 10) ds =
    Sequence.fold ds ~init:zero ~f:(fun acc n -> base * acc + n)

  let sum_digits ?(base = of_int_exn 10) n =
    let rec iter n acc =
      if n = zero
      then acc
      else iter (n / base) (n % base + acc)
    in
    iter n zero

  let rec gcd a b =
    if b = zero
    then a
    else gcd b (a % b)

  let lcm a b =
    a / (gcd a b) * b

  let factorial n =
    Sequence.unfold ~init:two ~f:(fun s -> Some (s, Int.succ s))
    |> Sequence.take_while ~f:((>=) n)
    |> Sequence.fold ~init:one ~f:( * )

  let next_probable_prime n =
    match n % of_int_exn 6 |> Int.to_int_exn with
    | 1 -> n + four
    | 5 -> n + two
    | 0 -> Int.succ n
    | 2 -> Int.succ n
    | 3 -> n + two
    | 4 -> Int.succ n
    | _ -> assert false

  let is_prime n =
    if n <= one
    then false
    else (
      let rec aux i =
        if i * i > n
        then true
        else if n % i = zero
        then false
        else aux (next_probable_prime i)
      in aux (of_int_exn 2))

  let rec next_prime n =
    let next = next_probable_prime n in
    if is_prime next
    then next
    else next_prime next

  let primes =
    Sequence.unfold ~init:two ~f:(fun p ->
      let next = next_prime p in
      Some (p, next))

  let factor =
    let rec aux i n =
      match i with
      | _ when i * i > n -> if n > one then [n] else []
      | _ when n % i = zero -> i :: aux i (n / i)
      | i -> aux (next_probable_prime i) n
    in
    aux two

  let prime_factor n = Util.run_length_encode (factor n)

  let divisors n =
    let mult_aux p a lst =
      let powers =
        Sequence.range ~stop:`inclusive 0 a
        |> Sequence.map ~f:of_int_exn
        |> Sequence.map ~f:(Int.pow p)
        |> Sequence.to_list
      in
      List.map ~f:(fun pp -> List.map ~f:(fun x -> pp * x) lst) powers
      |> List.concat
    in
    let rec div_aux pfs lst =
      match pfs with
      | [] -> lst
      | ((p, a)::tl) ->
        div_aux tl (mult_aux p a lst)
    in
    div_aux (prime_factor n) [one]

  let num_divisors n =
    prime_factor n
    |> List.fold ~init:one ~f:(fun acc (_, a) -> acc * (of_int_exn a + one))

  let totient n =
    prime_factor n
    |> List.fold ~init:one ~f:(fun acc (p, a) ->
      let pam1 = Int.pow p (of_int_exn a - one) in
      acc * pam1 * (p - one))

  let binomial n r =
    let top    = range ~stop:`inclusive (r + one) n in
    let bottom = range ~stop:`inclusive one (n - r) in
    Sequence.zip top bottom
    |> Sequence.fold ~init:one ~f:(fun acc (t, b) -> acc * t / b)

  let powmod a b ~modulus =
    if a < zero
    then (raise_s [%message "powmod negative base" (a : integer)]);
    let rec loop a b ac =
      if b = zero
      then ac
      else if b % two = zero
      then (loop (a * a % modulus) (b / two) ac)
      else (loop a (b - one) (ac * a % modulus))
    in
    loop a b one

  let bezout a b =
    let rec loop r0 s0 t0 r1 s1 t1 =
      if r1 = zero
      then (s0, t0, r0)
      else (
        let q = r0 / r1 in
        loop r1 s1 t1 (r0 - q * r1) (s0 - q * s1) (t0 - q * t1)
      )
    in
    loop a one zero b zero one

  let chinese_remainder_theorem residues =
    List.reduce_balanced_exn residues ~f:(fun (r1, m1) (r2, m2) ->
      let (s, t, g) = bezout m1 m2 in
      if g <> one
      then (
        raise_s [%message "moduli not coprime" (m1 : integer) (m2 : integer)]);
      let x = r1 * t * m2 + r2 * s * m1 in
      (x, m1 * m2))

  let fibonacci =
    Sequence.unfold ~init:(one, one) ~f:(fun (a, b) -> Some (a, (b, a + b)))

  let natural_numbers ?(init = zero) () =
    Sequence.unfold ~init ~f:(fun n -> Some (n, n + one))

  let isqrt n =
    let rec loop n =
      if n < two
      then n
      else (
        let small = loop (n asr 2) lsl 1 in
        let large = small + one in
        if large * large > n
        then small
        else large
      )
    in
    if n < zero
    then (failwiths "isqrt: negative number" n [%sexp_of: Int.t])
    else loop n

  let is_perfect_square n =
    let sqrt = isqrt n in
    sqrt * sqrt = n
end

module Int    = Make (Int)
module Bigint = Make (struct
    open Bignum.Std
    include Bigint
    (* different signature in Int_intf.S_unbounded *)
    let to_int64 _t =
      raise_s [%message "unimplemented" "to_int64"]
  end)

let prime_sieve limit =
  let len = limit + 1 in
  let primes = Array.create ~len true in
  let rec mark p n =
    if n < len
    then (
      primes.(n) <- false;
      mark p (n + p)
    )
  in
  let rec sieve p =
    if p * p < len
    then (
      if primes.(p)
      then mark p (p * p);
      sieve (Int.next_probable_prime p)
    )
  in
  primes.(0) <- false;
  primes.(1) <- false;
  sieve 2;
  primes

let factorial_prime_factor n =
  let primes = prime_sieve n in
  let factors = ref [] in
  Array.iteri primes ~f:(fun p is_prime ->
    if is_prime
    then (
      let rec loop n ac =
        if n = 0
        then ac
        else (loop (n / p) (n / p + ac))
      in
      factors := (p, loop n 0) :: !factors));
  List.rev !factors

(* TODO functorize *)
let multinomial xs =
  let p = ref 1 in
  let n = ref 1 in
  List.iter xs ~f:(fun x ->
    for i = 1 to x do
      p := !p * !n / i;
      incr n;
    done);
  !p
