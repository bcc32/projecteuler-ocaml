open Core
open Bignum.Std

module Number = struct
  module T = struct
    type t =
      { factors : int Bigint.Map.t
      ; value : Bigint.t
      ; lg2_divisors : int
      }
    [@@deriving sexp]

    let compare { value = a; _ } { value = b; _ } =
      Bigint.compare a b
  end
  include T

  let _invariant { factors; value; lg2_divisors = _ } =
    Map.iter_keys factors ~f:(fun p -> assert (Euler.Bigint.is_prime p));
    let prod =
      Map.fold factors ~init:Bigint.one ~f:(fun ~key ~data ac ->
        Bigint.(ac * pow key (of_int data))
      )
    in
    assert (value = prod)
      (* more stuff like all a should be 2^n-1, should have the right lg2_divisors *)

  let one = { factors = Bigint.Map.empty; value = Bigint.one; lg2_divisors = 0 }

  let double_divisors { factors; value; lg2_divisors } p =
    let value = ref value in
    let factors =
      Map.update factors p ~f:(function
        | None ->
          value := Bigint.(!value * p);
          1
        | Some a ->
          value := Bigint.(!value * pow p (of_int Int.(a + 1)));
          a * 2 + 1)
    in
    { factors; value = !value; lg2_divisors = lg2_divisors + 1 }

  (* reverse order *)
  let eligible_primes { factors; _ } =
    match Map.keys factors |> List.rev with
    | [] -> [ Bigint.of_int 2 ]
    | (hd :: _) as ps -> Euler.Bigint.next_prime hd :: ps

  include Comparable.Make(T)
end

module M = struct
  let problem_number = 500

  let main () =
    let n = ref Number.one in
    while !n.lg2_divisors < 500_500 do
      Debug.eprintf "%d" !n.lg2_divisors;
      Number.eligible_primes !n
      |> List.map ~f:(fun p -> Number.double_divisors !n p)
      |> List.min_elt ~cmp:Number.compare
      |> uw
      |> (:=) n
    done;
    printf !"%{sexp: Number.t}\n" !n
end
include Solution.Make(M)
