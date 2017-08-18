open Core

let modulo = 500_500_507

module Factor = struct
  module T = struct
    type t =
      { prime    : int
      ; exponent : int
      ; value    : int          (* modulo [modulo] *)
      ; log      : float
      }
    [@@deriving sexp]

    let compare = Comparable.lift Float.compare ~f:(fun t -> t.log)
  end

  include T
  include Comparable.Make(T)

  let of_prime prime =
    if not (Euler.Int.is_prime prime)
    then (raise_s [%message "not a prime" (prime : int)]);
    { prime
    ; exponent = 1
    ; value    = prime mod modulo
    ; log      = Float.log (Float.of_int prime)
    }

  let next t =
    { prime    = t.prime
    ; exponent = 2 * t.exponent
    ; value    = t.value * t.value mod modulo
    ; log      = 2. *. t.log
    }
end

module M = struct
  let problem_number = 500

  let power_of_two_divisors k =
    let queue = Heap.create ~cmp:Factor.compare () in
    Heap.add queue (Factor.of_prime 2);
    let number = ref 1 in
    for i = 1 to k do
      if i mod 1000 = 0
      then (
        Debug.eprintf "i %d" i
      );
      let factor = Heap.pop_exn queue in
      number := !number * factor.value mod modulo;
      Heap.add queue (Factor.next factor);
      if factor.exponent = 1
      then (
        let prime = Euler.Int.next_prime factor.prime in
        Heap.add queue (Factor.of_prime prime)
      )
    done;
    !number

  let main () =
    power_of_two_divisors 500_500
    |> printf "%d\n"
    (* 35407281, 48s *)
end
include Solution.Make(M)
