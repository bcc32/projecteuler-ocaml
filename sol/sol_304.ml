open! Core
open! Import

let a =
  lazy (
    let a1 = Number_theory.Int.next_prime 100_000_000_000_000 in
    Sequence.unfold ~init:a1 ~f:(fun a ->
      Debug.eprintf "%d" a;
      let a' = Number_theory.Int.next_prime a in
      Some (a', a')))
  |> Sequence.of_lazy
;;

let b = Sequence.map a ~f:Number_theory.Int.fast_fibonacci

module Int_modulo = struct
  type t = int [@@deriving sexp]

  let modulus = 1234567891011

  let zero = 0
  let (+) a b = (a + b) % modulus
  let (-) a b = (a - b) % modulus
end

module M = struct
  let problem = `Number 304

  let main () =
    Sequence.take b 100_000
    |> Sequence.sum (module Int_modulo) ~f:Fn.id
    |> printf "%d\n"
  ;;
end

include Solution.Make(M)
