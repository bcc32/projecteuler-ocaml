open Core.Std
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

(** NUMBER THEORY **)

(* returns the digits of the number *)
let digits_of_string n =
  let zero = Char.to_int '0' in
  String.to_list_rev n
  |> List.rev_map ~f:(fun c -> Char.to_int c - zero)

(* return a list of the prime factors of n *)
let factor =
  let rec aux i n =
    match i with
    | _ when i * i > n -> if n > 1 then [n] else []
    | _ when n mod i = 0 -> i :: aux i (n / i)
    | 2 -> aux 3 n
    | _ -> aux (i + 2) n
  in aux 2

(* return a list of the prime factors of n with multiplicities *)
let prime_factor n = run_length_encode (factor n)

(* return a (non-sorted) list of divisors of n *)
let divisors n =
  let mult_aux p a lst =
    let powers =
      List.range ~stop:`inclusive 0 a
      |> List.map ~f:(Int.pow p)
    in
    List.concat begin
      List.map ~f:(fun pp -> List.map ~f:(fun x -> pp * x) lst) powers
    end
  in
  let rec div_aux pfs lst =
    match pfs with
    | [] -> lst
    | ((p, a)::tl) ->
      div_aux tl (mult_aux p a lst)
  in div_aux (prime_factor n) [1]

(* [num_divisors n] returns the number of divisors of n *)
let num_divisors n =
  List.fold_left ~f:(fun acc (_, a) -> acc * (a + 1)) ~init:1 (prime_factor n)

(** GEOMETRY **)
let is_pythagorean_triple a b c = a * a + b * b = c * c

module Number_theory = struct
  module type S = sig
    type int
    val range
      :  ?stride:int
      -> ?start:[ `inclusive | `exclusive ]
      -> ?stop:[ `exclusive | `inclusive ]
      -> int -> int -> int Sequence.t
    val digits_of_int : ?base:int -> int -> int list
    val int_of_digits : ?base:int -> int Sequence.t -> int
    val sum_digits : ?base:int -> int -> int
    val factorial : int -> int
    val is_prime : int -> bool
    val next_probable_prime : int -> int
    val next_prime : int -> int
    val primes : int Sequence.t
    val fibonacci : int Sequence.t
    val binomial : int -> int -> int
    val natural_numbers : ?init:int -> unit -> int Sequence.t
  end

  module Make(Int : Int_intf.S) : S with type int = Int.t = struct
    open Int.O

    type int = Int.t

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
        Some (next, next))

    let fibonacci =
      Sequence.unfold ~init:(one, one) ~f:(fun (a, b) -> Some (a, (b, a + b)))

    let binomial n r =
      let top    = range ~stop:`inclusive (r + one) n in
      let bottom = range ~stop:`inclusive one (n - r) in
      Sequence.zip top bottom
      |> Sequence.fold ~init:one ~f:(fun acc (t, b) -> acc * t / b)

    let natural_numbers ?(init = zero) () =
      Sequence.unfold ~init ~f:(fun n -> Some (n, n + one))
  end
end

module Int    = Number_theory.Make(Int)
module Bigint = Number_theory.Make(
struct
  include Bigint
  let num_bits = 0
  let min_value = zero
  let max_value = zero
  let shift_right_logical _t _b =
    raise_s [%message "unimplemented" "shift_right_logical"]
  let to_int64 _t =
    raise_s [%message "unimplemented" "to_int64"]
end)
