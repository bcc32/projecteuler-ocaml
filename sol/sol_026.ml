open! Core
open! Import

module M = struct
  let problem = `Number 026

  let rec divide_through k n =
    Bigint.(if n % k = zero then divide_through k (n / k) else n)
  ;;

  let cycle_length n =
    let n =
      n
      |> Bigint.of_int
      |> divide_through (Bigint.of_int 2)
      |> divide_through (Bigint.of_int 5)
    in
    let rec loop d c =
      Bigint.(if d % n = zero then c else loop ((of_int 10 * d) + of_int 9) Int.(c + 1))
    in
    loop (Bigint.of_int 9) 1
  ;;

  let main () =
    Sequence.range 1 1000
    |> Sequence.map ~f:(fun n -> n, cycle_length n)
    |> Sequence.max_elt ~compare:(Comparable.lift Int.compare ~f:snd)
    |> uw
    |> fst
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
