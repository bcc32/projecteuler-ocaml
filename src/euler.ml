open Core.Std

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

(** NUMBER THEORY **)

(* returns the digits of the number *)
let digits_of_int n =
  let rec aux n d = if n = 0 then d else aux (n / 10) (n mod 10 :: d) in
  aux n []

(* XXX deprecated
   let digits_of_big_int n =
   let big_ten = Big_int.big_int_of_int 10 in
   let rec aux n d =
   if Big_int.eq_big_int n Big_int.zero_big_int then
   d
   else
   let (q, r) = Big_int.quomod_big_int n big_ten in
   aux q (Big_int.int_of_big_int r :: d)
   in aux n []
*)

let digits_of_string n =
  let zero = Char.to_int '0' in
  String.to_list_rev n
  |> List.rev_map ~f:(fun c -> Char.to_int c - zero)

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
    let powers = (List.map ~f:(fun k -> Int.pow p k) (List.range 0 a)) in
    ListLabels.flatten begin
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
