open Core
open Bignum.Std

(* run length encoding (item, count) *)
let rec run_length_encode lst =
  let rec count item lst foo =
    match lst with
    | [] -> foo
    | (hd::tl) when hd = item -> count item tl (foo + 1)
    | _ -> foo
  in
  let rec drop item lst =
    match lst with
    | [] -> []
    | (hd::tl) when hd = item -> drop item tl
    | _ -> lst
  in
  match lst with
  | [] -> []
  | (hd::_) -> (hd, count hd lst 0) :: run_length_encode (drop hd lst)

let is_palindrome l ~equal =
  List.equal ~equal l (List.rev l)

let next_permutation ~cmp a =
  let a = Array.copy a in
  with_return_option (fun { return } ->
    for i = Array.length a - 2 downto 0 do
      if cmp a.(i) a.(i + 1) < 0
      then (
        let min_index = ref (i + 1) in
        for j = i + 2 to Array.length a - 1 do
          if cmp a.(i) a.(j) < 0 && cmp a.(j) a.(!min_index) < 0
          then min_index := j
        done;
        Array.swap a i !min_index;
        Array.sort ~pos:(i + 1) ~cmp a;
        return a
      )
    done
  )

let permutations ~cmp l =
  let a = Array.sorted_copy ~cmp (List.to_array l) in
  let next_permutations =
    Sequence.unfold ~init:a ~f:(fun a ->
      let open Option.Let_syntax in
      let%map next = next_permutation ~cmp a in
      (Array.to_list next, next)
    )
  in
  Sequence.shift_right next_permutations (Array.to_list a)

(* returns the digits of the number *)
let digits_of_string n =
  let zero = Char.to_int '0' in
  String.to_list_rev n
  |> List.rev_map ~f:(fun c -> Char.to_int c - zero)

(** GEOMETRY **)
let is_pythagorean_triple a b c = a * a + b * b = c * c

module Number_theory = struct
  module type S = sig
    type integer
    val range
      :  ?stride:integer
      -> ?start:[ `inclusive | `exclusive ]
      -> ?stop:[ `exclusive | `inclusive ]
      -> integer -> integer -> integer Sequence.t
    val digits_of_int : ?base:integer -> integer -> integer list
    val int_of_digits : ?base:integer -> integer Sequence.t -> integer
    val sum_digits : ?base:integer -> integer -> integer
    val factorial : integer -> integer
    val is_prime : integer -> bool
    val next_probable_prime : integer -> integer
    val next_prime : integer -> integer
    val primes : integer Sequence.t
    val factor : integer -> integer list
    val prime_factor : integer -> (integer * int) list
    val divisors : integer -> integer list
    val num_divisors : integer -> integer
    val totient : integer -> integer
    val binomial : integer -> integer -> integer
    val fibonacci : integer Sequence.t
    val natural_numbers : ?init:integer -> unit -> integer Sequence.t
  end

  module Make(Int : Int_intf.S_unbounded) : S with type integer = Int.t = struct
    open Int.O

    type integer = Int.t

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

    let prime_factor n = run_length_encode (factor n)

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

    let fibonacci =
      Sequence.unfold ~init:(one, one) ~f:(fun (a, b) -> Some (a, (b, a + b)))

    let natural_numbers ?(init = zero) () =
      Sequence.unfold ~init ~f:(fun n -> Some (n, n + one))
  end
end

module Int    = Number_theory.Make(Int)
module Bigint = Number_theory.Make(struct
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

module Numerics = struct
  module type Real_intf = sig
    type t
    val abs : t -> t
    val (+)   : t -> t -> t
    val (-)   : t -> t -> t
    val ( * ) : t -> t -> t
    val (/)   : t -> t -> t

    val of_int : int -> t

    val sign : t -> Sign.t

    include Comparable.S with type t := t
  end

  module type S = sig
    type real
    val bisect
      : f : (real -> real)
      -> epsilon : real
      -> low     : real
      -> high    : real
      -> real
    val newton's_method
      :  f  : (real -> real)
      -> f' : (real -> real)
      -> epsilon : real
      -> init    : real
      -> real
  end

  module Make(Real : Real_intf) : S with type real = Real.t = struct
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
              end
          )
        in
        let y_lo = f x_lo in
        let y_hi = f x_hi in
        loop x_lo x_hi y_lo y_hi

    let rec newton's_method ~f ~f' ~epsilon ~init =
      let delta = f init / f' init in
      if abs delta < epsilon
      then init
      else newton's_method ~f ~f' ~epsilon ~init:(init - delta)
  end
end

module Float  = Numerics.Make(Float)
module Bignum = Numerics.Make(struct
    include Bignum
    let sign t = Sign.of_int (sign t)
  end)
