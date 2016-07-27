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
          if a.(i) < a.(j) && a.(j) < a.(!min_index)
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
let digits_of_int ?(base = 10) n =
  let rec aux n d =
    if n = 0
    then d
    else aux (n / base) (n mod base :: d)
  in
  aux n []

let digits_of_string n =
  let zero = Char.to_int '0' in
  String.to_list_rev n
  |> List.rev_map ~f:(fun c -> Char.to_int c - zero)

let sum_digits ?(base = Bigint.of_int 10) n =
  let open Bigint in
  let rec iter n acc =
    if n = zero
    then acc
    else iter (n / base) (n % base + acc)
  in
  iter n zero

let factorial n =
  Sequence.range 2 n
  |> Sequence.fold ~init:Bigint.one ~f:(fun acc x ->
    Bigint.(acc * of_int x)
  )

(* returns true iff the argument is prime *)
let is_prime =
  let rec aux i n =
    match i with
    | _ when i * i > n -> true
    | _ when n mod i = 0 -> false
    | 2 -> aux 3 n
    | _ -> aux (i + 2) n
  in aux 2

let next_probable_prime n =
  match n mod 6 with
  | 1 -> n + 4
  | 5 -> n + 2
  | 0 -> n + 1
  | 2 -> n + 1
  | 3 -> n + 2
  | 4 -> n + 1
  | _ -> assert false

(* return the smallest prime greater than the argument *)
let rec next_prime n =
  let next = next_probable_prime n in
  if is_prime next
  then next
  else next_prime next

let primes =
  Sequence.unfold ~init:2 ~f:(fun p ->
    let next = next_prime p in
    Some (next, next)
  )

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

let fibonacci =
  Sequence.unfold ~init:(Bigint.one, Bigint.one) ~f:(fun (a, b) ->
    Some (a, (b, Bigint.(a + b)))
  )

let binomial n r =
  let top    = Sequence.range ~stop:`inclusive (r + 1) n in
  let bottom = Sequence.range ~stop:`inclusive 1 (n - r) in
  Sequence.zip top bottom
  |> Sequence.fold ~init:Bigint.one ~f:(fun acc (t, b) ->
    Bigint.(acc * of_int t / of_int b)
  )

let natural_numbers ?(init = 0) () =
  Sequence.unfold ~init ~f:(fun n -> Some (n, n + 1))

(** GEOMETRY **)
let is_pythagorean_triple a b c = a * a + b * b = c * c
