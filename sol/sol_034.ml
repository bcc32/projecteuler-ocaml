open! Core
open! Import

module M = struct
  let problem = Number 34

  (* Derived by observing that 9! * n < 10^(n - 1) for all n > 7. *)
  let max_digits = 7

  let incr digits =
    let rec loop i =
      if i >= 0
      then
        if digits.(i) = 9
        then (
          digits.(i) <- 0;
          loop (i - 1))
        else digits.(i) <- digits.(i) + 1
      else failwith "no digits to increment"
    in
    loop (Array.length digits - 1)
  ;;

  let main () =
    let factorial = Array.init 10 ~f:Number_theory.Int.factorial in
    let digits = Array.create ~len:max_digits 0 in
    (* start at 10 *)
    digits.(Array.length digits - 1) <- 9;
    let sum = ref 0 in
    for num_digits = 2 to max_digits do
      for n = Int.pow 10 (num_digits - 1) to Int.pow 10 num_digits - 1 do
        incr digits;
        if debug
        then (
          let n' = Array.fold digits ~init:0 ~f:(fun acc x -> (acc * 10) + x) in
          [%test_eq: int] n n');
        let n' =
          (* actually substantially faster than Array.sum *)
          let sum = ref 0 in
          for i = Array.length digits - num_digits to Array.length digits - 1 do
            sum := !sum + factorial.(digits.(i))
          done;
          !sum
        in
        if n = n'
        then (
          if debug then Debug.eprintf "%d" n;
          sum := !sum + n)
      done
    done;
    printf "%d\n" !sum
  ;;
end

include Solution.Make (M)
