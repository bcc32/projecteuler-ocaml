open! Core
open! Import

module M = struct
  let problem = `Number 293
  let is_prime = lazy (Number_theory.prime_sieve 100_000)

  let is_admissible n =
    if n % 1_000_000 = 0 then Debug.eprintf !"%{Int#hum}" n;
    let rec start n i =
      if n = 1
      then true
      else if (force is_prime).(i)
      then if n mod i <> 0 then false else continue n i
      else start n (Number_theory.Int.next_probable_prime i)
    and continue n i = if n mod i = 0 then continue (n / i) i else start n (i + 1) in
    start n 2
  ;;

  let pseudo_fortunate n =
    let rec loop i = if Number_theory.Int.is_prime (n + i) then i else loop (i + 1) in
    loop 2
  ;;

  let main () =
    Number_theory.Int.natural_numbers ~init:2 ()
    |> Sequence.take_while ~f:(fun x -> x < 1_000_000_000)
    |> Sequence.filter ~f:is_admissible
    |> Sequence.map ~f:pseudo_fortunate
    |> Sequence.fold ~init:Int.Set.empty ~f:Set.add
    |> Set.sum (module Int) ~f:Fn.id
    |> printf "%d\n"
  ;;

  (* 2209
     2.3m *)
end

include Solution.Make (M)
