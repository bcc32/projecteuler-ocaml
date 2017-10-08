open! Core
open Bignum.Std

module M = struct
  let problem_number = 29

  let main () =
    let powers = Bigint.Hash_set.create () in
    for a = 2 to 100 do
      for b = 2 to 100 do
        let power = Bigint.pow (Bigint.of_int a) (Bigint.of_int b) in
        Hash_set.add powers power
      done
    done;
    printf "%d\n" (Hash_set.length powers)
  ;;
end

include Euler.Solution.Make(M)
