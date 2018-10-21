open! Core
open! Import

module M = struct
  let problem = Number 323
  let word_size = 32

  (* probability of transitioning from [a] 1 bits to [b] 1 bits *)
  let p_transition a b =
    assert (a <= b);
    assert (a < 32);
    assert (b <= 32);
    let zero_bits = word_size - a in
    let diff = b - a in
    float (Number_theory.Int.binomial zero_bits diff) /. (2. ** float zero_bits)
  ;;

  let rec expectation_to_32 =
    let cache = Int.Table.create () in
    fun n ->
      if n = 32
      then 0.0
      else
        Hashtbl.findi_or_add cache n ~default:(fun n ->
          let e = ref 1. in
          (* E[n] = 1 + P[n] E[n] + P[n + 1]E[n + 1] + ... + P[32]E[32] *)
          (* E[n] = (P[n + 1]E[n + 1] + ... + P[32]E[32]) / (1 - P[n]) *)
          for i = n + 1 to 32 do
            e := !e +. (p_transition n i *. expectation_to_32 i)
          done;
          !e /. (1. -. p_transition n n))
  ;;

  let main () = printf "%.10f\n" @@ expectation_to_32 0

  (* 6.3551758451
     0.747ms *)
end

include Solution.Make (M)
