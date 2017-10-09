open! Core
open! Import

module M = struct
  let problem = `Number 549

  let limit =
    match Sys.getenv "LIMIT" with
    | Some x -> Int.of_string x
    | None -> 100_000_000
  ;;

  let divide n p =
    let rec loop n ac =
      if n mod p = 0
      then (loop (n / p) (ac + 1))
      else ac
    in
    loop n 0
  ;;

  (* Sieve inspired by Nore's solution at https://projecteuler.net/thread=549#235492. *)
  let sieve n =
    let m = Array.create 0 ~len:(n + 1) in
    for p = 2 to n do
      if m.(p) = 0              (* prime *)
      then (
        (* loop invariant: [smallest] is the smallest multiple of [p] such that
           [power | smallest!]. [k] is incremented each loop and serves to
           adjust [smallest] when needed. *)
        let rec loop_powers power k smallest =
          if power <= n
          then (
            (* Debug.eprintf "power %d" power; *)
            let (k, smallest) =
              let rec ensure_enough_p k smallest =
                if k <= 0
                then (k, smallest)
                else (
                  let smallest = smallest + p in
                  ensure_enough_p (k - divide smallest p) smallest)
              in
              ensure_enough_p k smallest
            in
            let rec loop j =
              if j <= n
              then (
                if smallest > m.(j)
                then (m.(j) <- smallest);
                loop (j + power))
            in
            loop power;
            loop_powers (power * p) (k + 1) smallest)
        in
        loop_powers p 1 0)
    done;
    Array.sum (module Int) m ~f:Fn.id
  ;;

  let main () =
    sieve limit
    |> printf "%d\n"
  ;;
  (* 476001479068717, 9.4s *)
end

include Solution.Make(M)
