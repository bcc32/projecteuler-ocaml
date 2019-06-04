open! Core
open! Import
module D = Distribution.Float

(* For the case of just two kinds of bacteria:

   - pdA = the probability that the descendants of a single A cell will die off
   - pdB = the probability that the descendants of a single B cell will die off

   pdA = 1/2(pdA^2) + 1/2(pdB^3)

   pdB = 1/2(pdA * pdB) + 1/2

   We can solve this by just finding the fixed point of pd*, with some appropriate initial
   condition (e.g., p=0 or p=0.5). *)

module State = struct
  type t =
    { mutable curr : float array
    ; mutable prev : float array
    }

  let create len =
    let curr = Array.create ~len 0. in
    let prev = Array.create ~len 0. in
    { curr; prev }
  ;;

  let swap t =
    let curr = t.curr in
    t.curr <- t.prev;
    t.prev <- curr
  ;;

  let iterate t ~n ~step =
    for _ = 1 to n do
      swap t;
      let prev : (float, read) Array.Permissioned.t =
        Array.Permissioned.of_array_id t.prev
      in
      for i = 0 to Array.length t.curr - 1 do
        t.curr.(i) <- step i prev
      done
    done
  ;;

  let overall_prob_death t = t.curr.(0)
end

let find_fixed_point ~k ~m ~iterations =
  let r : (int, read) Array.Permissioned.t =
    let r = Array.create 0 ~len:(k * m) in
    r.(0) <- 306;
    for i = 1 to Array.length r - 1 do
      let r' = r.(i - 1) in
      r.(i) <- r' * r' mod 10_007
    done;
    Array.Permissioned.of_array_id r
  in
  let step i ps =
    let module Array = Array.Permissioned in
    let sum = ref 0. in
    for j = 0 to m - 1 do
      sum :=
        !sum
        +.
        match r.((i * m) + j) mod 5 with
        | 0 -> 1.
        | 1 -> ps.(i) ** 2.
        | 2 -> ps.(2 * i mod k)
        | 3 -> ps.(((i * i) + 1) mod k) ** 3.
        | 4 -> ps.(i) *. ps.((i + 1) mod k)
        | _ -> assert false
    done;
    !sum /. float m
  in
  let state = State.create k in
  State.iterate state ~n:iterations ~step;
  State.overall_prob_death state
;;

let%expect_test "basic example S(2,2)" =
  printf "%.8f\n" (find_fixed_point ~k:2 ~m:2 ~iterations:100);
  [%expect {| 0.07243802 |}]
;;

let problem = Number 666

let main () =
  let k = 500 in
  let m = 10 in
  printf "%.8f\n" (find_fixed_point ~k ~m ~iterations:100)
;;

(* 14.582934ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 0.48023168 |}]
;;

include (val Solution.make ~problem ~main)
