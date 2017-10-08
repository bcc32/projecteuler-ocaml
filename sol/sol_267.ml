open! Core
open Bignum.Std

module M = struct
  let problem_number = 267

  (* Each coin flip, all possible starting values [x] result in two equally
     likely outcomes: [x * (1 + 2 * f)] and [x * (1 - f)].

     The distribution for each step evolves over time like so (let [a = 1 - f]
     and [b = 1 + 2 * f]):

     | Flips | Distribution
     |     0 | { a^0 b^0 : 1 }
     |     1 | { a^0 b^1 : 0.5, a^1 b^0 : 0.5 }
     |     2 | { a^0 b^2 : 0.25, a^1 b^1 : 0.5, a^2 b^0 : 0.25 }
     |   ... | ...

     That is to say, the pattern parallels the binomial formula. So, we can
     determine the probability [Pr(money >= 10^9 at 1000 flips)] as a function
     of [f] rather straightforwardly. *)

  let flips = 1000

  let denominator = Bignum.(of_int 2 ** flips)

  let p_billionaire f =
    let accum = ref Bigint.zero in
    let a = Bignum.(one - f) in
    let b = Bignum.(one + f + f) in
    let ratio = Bignum.(b / a) in
    let value = ref Bignum.(a**flips) in
    for i = 0 to flips do
      (* This was originally [!value >= Bignum.billion], which accidentally used
         polymorphic comparison instead of Bignum.(>=), leading to nasty bugs.
         *)
      if Bignum.(!value >= billion)
      then begin
        let coeff = Euler.Bigint.binomial (Bigint.of_int flips) (Bigint.of_int i) in
        accum := Bigint.(!accum + coeff)
      end;
      value := Bignum.(!value * ratio)
    done;
    Bignum.(of_bigint !accum / denominator)
  ;;

  let main () =
    let _ = p_billionaire in
    failwith "unimplemented"
  ;;
end
include Euler.Solution.Make(M)
