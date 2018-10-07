open! Core
open! Import

module M = struct
  let problem = Number 24

  let main () =
    let sequence = Array.init 10 ~f:Fn.id in
    for _ = 1 to 999_999 do
      assert (Sequences.next_permutation_inplace sequence ~compare:Int.compare)
    done;
    sequence
    |> Array.map ~f:Int.to_string
    |> String.concat_array ~sep:""
    |> printf "%s\n"
  ;;
end

include Solution.Make (M)
