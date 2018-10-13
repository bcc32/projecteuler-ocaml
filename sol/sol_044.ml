open! Core
open! Import

let is_pentagonal n =
  let s = Number_theory.Int.isqrt (1 + (24 * n)) in
  s * s = 1 + (24 * n) && s % 6 = 5
;;

module M = struct
  let problem = Number 44

  let main () =
    with_return (fun { return } ->
      for k = 1 to 1_000_000 do
        for j = k downto 1 do
          let pk = k * ((3 * k) - 1) / 2 in
          let pj = j * ((3 * j) - 1) / 2 in
          if is_pentagonal (pk + pj) && is_pentagonal (pk - pj) then return (pk - pj)
        done
      done;
      assert false)
    |> printf "%d\n"
  ;;

  (* 5482660
     699.771ms *)
end

include Solution.Make (M)
